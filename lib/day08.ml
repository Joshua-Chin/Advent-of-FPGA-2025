open! Base
open! Hardcaml
open! Signal

let elem_bits = 17 (* Up to 5 digits *)
let output_bits = 50
let dist_bits = 2 * elem_bits
let max_vectors = 1024
let max_dim = 3
let addr_bits = address_bits_for max_vectors

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    data_in : 'a; [@bits 8]
    data_in_valid : 'a;
    finish : 'a;
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    part1 : 'a; [@bits output_bits]
    part2 : 'a; [@bits output_bits]
    part1_valid : 'a;
    part2_valid : 'a;
  }
  [@@deriving hardcaml]
end

let distance a b =
  List.map2_exn a b ~f:(fun a b ->
      let d = a -: b in
      d *+ d)
  |> reduce ~f:Signal.( +: )

module Edge = struct
  type 'a t = {
    before : 'a; [@bits addr_bits]
    idx : 'a; [@bits addr_bits]
    dist : 'a; [@bits dist_bits]
  }
  [@@deriving hardcaml]
end

module Parser = struct
  module O = struct
    type 'a t = {
      elmenents : 'a list; [@length 3] [@bits elem_bits]
      valid : 'a;
    }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid; _ } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let output = O.Of_always.reg spec in
    let%hw_var idx = Variable.reg spec ~width:2 in
    let%hw_var decimal = Variable.reg spec ~width:elem_bits in

    let assign_element =
      proc
        [
          List.mapi output.elmenents ~f:(fun i r ->
              when_ (idx.value ==:. i) [ r <-- decimal.value ])
          |> proc;
          decimal <--. 0;
        ]
    in

    compile
      [
        output.valid <-- gnd;
        when_ data_in_valid
          [
            Util.try_parse_to_digit decimal data_in;
            when_
              (data_in ==:. Char.to_int ',')
              [ assign_element; idx <-- idx.value +:. 1 ];
            when_
              (data_in ==:. Char.to_int '\n')
              [ assign_element; output.valid <-- vdd; idx <--. 0 ];
          ];
      ];
    O.Of_always.value output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"parser" create
end

module Part2Solver = struct
  module I = struct
    type 'a t = { clock : 'a; clear : 'a; finish : 'a; input : 'a Parser.O.t }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { solution : 'a; [@bits output_bits] valid : 'a }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Loading
      | Prepare_execute
      | Update_distances
      | Compute_min_distance
      | Add_new_point
      | Compute_output
      | Finish
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Compute_output_states = struct
    type t = Retrieve_x0 | Retrieve_x1 | Combine_xs
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) ({ clock; clear; finish; input } : _ I.t) : _ O.t
      =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    let output_sm = State_machine.create (module Compute_output_states) spec in
    (* Store the vectors in shift buffers for parallel comparison *)
    let vectors_reg =
      Array.init max_vectors ~f:(fun _ ->
          List.init max_dim ~f:(fun _ -> Variable.reg spec ~width:elem_bits))
    in
    (* Store the vectors in RAM for lookup *)
    let module SDPR = Util.SimpleDualPortRam (struct
      let size = max_vectors
      let item_width = max_dim * elem_bits
    end) in
    let vectors_ram = SDPR.I.Of_always.wire zero in
    let vectors_ram_out =
      SDPR.create scope ~clock (SDPR.I.Of_always.value vectors_ram)
    in

    let vectors_idx = Variable.reg spec ~width:addr_bits in
    let last_idx = Variable.reg spec ~width:addr_bits in

    (* Track the number of connected components *)
    let%hw_var connected_components = Variable.reg spec ~width:addr_bits in

    (* Stage: Update Distances *)

    (* Track the minimum distance and edge index between 
   the current connected component and all each point *)
    let min_distances =
      Array.init max_vectors ~f:(fun idx ->
          (* Distance should default to infinity, expect for the first node *)
          let default = (if idx = 0 then zero else ones) dist_bits in
          ( Variable.reg spec ~width:addr_bits,
            Variable.reg
              (Reg_spec.override ~clear_to:default spec)
              ~width:dist_bits ))
    in

    (* Stage: Compute Min Distance *)
    let new_point = Edge.Of_always.reg spec in

    (* Stage: add new point *)
    let max_mst_edge = Edge.Of_always.reg spec in

    (* Stage: compute output *)
    let%hw_var x0 = Variable.reg spec ~width:elem_bits in
    let%hw_var part2 = Variable.reg spec ~width:output_bits in
    let%hw_var valid = Variable.reg spec ~width:1 in

    let prepare_load_vector idx =
      proc
        [
          vectors_ram.read_enable <-- vdd;
          vectors_ram.read_address <-- last_idx.value -: idx;
          vectors_idx <-- idx;
        ]
    in

    compile
      [
        sm.switch
          [
            (* Load the vectors into memory *)
            ( Loading,
              [
                when_ input.valid
                  [
                    (* Write the vector into the shift buffer register *)
                    Array.mapi vectors_reg ~f:(fun i r ->
                        if i = 0 then List.map2_exn r input.elmenents ~f:( <-- )
                        else
                          List.map2_exn r
                            vectors_reg.(i - 1)
                            ~f:(fun r l -> r <-- l.value))
                    |> Array.map ~f:proc |> Array.to_list |> proc;
                    (* Write the vector into RAM *)
                    vectors_ram.write_enable <-- vdd;
                    vectors_ram.write_address <-- vectors_idx.value;
                    vectors_ram.write_data <-- reduce input.elmenents ~f:( @: );
                    vectors_idx <-- vectors_idx.value +:. 1;
                    last_idx <-- vectors_idx.value;
                  ];
                when_ finish [ sm.set_next Prepare_execute ];
              ] );
            (* Add a stage to allow the RAM / register writes to settle. This probably can be optimized away. *)
            ( Prepare_execute,
              [
                sm.set_next Update_distances;
                connected_components <-- vectors_idx.value;
                prepare_load_vector (zero addr_bits);
              ] );
            (* Update the minimum distances *)
            ( Update_distances,
              [
                Array.mapi (Array.zip_exn vectors_reg min_distances)
                  ~f:(fun idx (v1, (i, d)) ->
                    let v0 = split_msb ~part_width:elem_bits vectors_ram_out in
                    let dist =
                      distance v0 (List.map v1 ~f:(fun x -> x.value))
                    in
                    when_
                      (dist <: d.value &: (last_idx.value >=:. idx))
                      [ i <-- vectors_idx.value; d <-- dist ])
                |> Array.to_list |> proc;
                sm.set_next Compute_min_distance;
              ] );
            (* Compute the minimum distance over all points *)
            ( Compute_min_distance,
              [
                (let min_dist_results =
                   Array.mapi min_distances ~f:(fun i (j, d) ->
                       {
                         Edge.idx =
                           Signal.of_int ~width:(address_bits_for max_vectors) i;
                         before = j.value;
                         (* If the distance is zero, we default to the max distance *)
                         dist =
                           mux2 (d.value ==:. 0) (ones (width d.value)) d.value;
                       })
                 in
                 let min_dist_result =
                   tree
                     (Array.to_list min_dist_results)
                     ~arity:2
                     ~f:
                       (reduce ~f:(fun l r ->
                            Edge.map2 l r ~f:(mux2 (l.dist <: r.dist))))
                 in
                 Edge.Of_always.assign new_point min_dist_result);
                sm.set_next Add_new_point;
              ] );
            ( Add_new_point,
              [
                (* Track the longest edge in the MST *)
                when_
                  (new_point.dist.value >: max_mst_edge.dist.value)
                  [
                    Edge.Of_always.assign max_mst_edge
                      (Edge.Of_always.value new_point);
                  ];
                (* Prepare for the next loop *)
                (let new_components = connected_components.value -:. 1 in
                 if_ (new_components ==:. 1)
                   [ sm.set_next Compute_output ]
                   [
                     sm.set_next Update_distances;
                     connected_components <-- new_components;
                     prepare_load_vector new_point.idx.value;
                   ]);
              ] );
            ( Compute_output,
              [
                output_sm.switch
                  [
                    ( Retrieve_x0,
                      [
                        output_sm.set_next Retrieve_x1;
                        prepare_load_vector max_mst_edge.before.value;
                      ] );
                    ( Retrieve_x1,
                      [
                        output_sm.set_next Combine_xs;
                        x0
                        <-- List.hd_exn
                              (split_msb ~part_width:elem_bits vectors_ram_out);
                        prepare_load_vector max_mst_edge.idx.value;
                      ] );
                    ( Combine_xs,
                      [
                        sm.set_next Compute_output;
                        (let x1 =
                           List.hd_exn
                             (split_msb ~part_width:elem_bits vectors_ram_out)
                         in
                         proc
                           [
                             part2 <-- uresize (x0.value *: x1) output_bits;
                             valid <-- vdd;
                           ]);
                      ] );
                  ];
              ] );
            (Finish, []);
          ];
      ];
    { solution = part2.value; valid = valid.value }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"part2" create
end

module States = struct
  type t = Finding_smallest_edges
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module MinEdgeStates = struct
  type t = Compute_distances | Check_against_max | Insert_edges
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create (scope : Scope.t) ({ clock; clear; finish; _ } as input : _ I.t) :
    _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let input = Parser.hierarchical scope input in

  let open Always in
  let sm = State_machine.create (module States) spec in
  let sm_min_edge = State_machine.create (module MinEdgeStates) spec in
  (* Track the vectors *)
  let%hw_var total_vectors = Variable.reg spec ~width:addr_bits in
  let vectors =
    Array.init max_vectors ~f:(fun _ ->
        List.init 3 ~f:(fun _ -> Variable.reg spec ~width:elem_bits))
  in

  (* Stage: Track the 1000 smallest edges *)
  let min_edge_idx =
    Variable.reg
      (Reg_spec.override ~clear_to:(one addr_bits) spec)
      ~width:addr_bits
  in
  let distances =
    Array.init max_vectors ~f:(fun _ ->
        (Variable.reg spec ~width:1, Variable.reg spec ~width:dist_bits))
  in
  let sorted_edges =
    Array.init 1000 ~f:(fun _ ->
        {
          (Edge.Of_always.reg spec) with
          (* The distance must default to the max value *)
          Edge.dist =
            Variable.reg
              (Reg_spec.override spec ~clear_to:(ones dist_bits))
              ~width:dist_bits;
        })
  in

  let is_finish = Variable.reg spec ~width:1 in
  compile
    [
      (* Input Handling *)
      when_ finish [ is_finish <-- vdd ];
      when_ input.valid
        [
          total_vectors <-- total_vectors.value +:. 1;
          (* Add to the shift buffer *)
          Array.mapi vectors ~f:(fun i r ->
              if i = 0 then List.map2_exn r input.elmenents ~f:( <-- )
              else List.map2_exn r vectors.(i - 1) ~f:(fun r v -> r <-- v.value))
          |> Array.map ~f:proc |> Array.to_list |> proc;
        ];
      (* Core Logic *)
      sm.switch
        [
          ( Finding_smallest_edges,
            [
              sm_min_edge.switch
                [
                  ( Compute_distances,
                    [
                      when_ (min_edge_idx)
                      Array.mapi (Array.zip_exn distances vectors)
                        ~f:(fun idx ((valid, dist), vec) -> when_)
                      |> Array.to_list |> proc;
                    ] );
                ];
            ] );
        ];
    ];
  {
    part1 = zero output_bits;
    part1_valid = gnd;
    part2 = zero output_bits;
    part2_valid = gnd;
  }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day08" create

(*  let input = Parser.hierarchical scope input in
  let part2 = Part2Solver.hierarchical scope {clock; clear; finish; input} in *)
