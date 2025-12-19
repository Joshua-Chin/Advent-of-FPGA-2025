open! Base
open! Hardcaml
open! Signal

let max_dim = 10
let max_buttons = 13
let output_bits = 16
let max_free_variables = 4

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    data_in : 'a; [@bits 8]
    data_in_valid : 'a;
  }
  [@@deriving hardcaml]
end

module Parser = struct
  module I = I

  module O = struct
    type 'a t = {
      (* Start a new problem *)
      new_problem : 'a;
      target : 'a; [@bits max_dim]
      (* Add a new button to the problem *)
      new_button : 'a;
      button : 'a; [@bits max_dim]
      (* Declare the current problem finished *)
      finish : 'a;
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Parse_new_problem | Parse_button | Wait_for_new_problem
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    let output = O.Of_always.wire zero in

    (* Generic accumulators *)
    let%hw_var decimal = Variable.reg spec ~width:(address_bits_for max_dim) in
    let%hw_var bit_accum = Variable.reg spec ~width:max_dim in

    (* Stage Parse New Problem *)
    let%hw_var target_idx =
      Variable.reg spec ~width:(address_bits_for max_dim)
    in

    let is_input c = data_in ==:. Char.to_int c in

    let new_bit_accum idx =
      bit_accum.value |: uresize (binary_to_onehot idx) max_dim
    in

    let clear = proc [ decimal <--. 0; bit_accum <--. 0; target_idx <--. 0 ] in

    compile
      [
        when_ data_in_valid
          [
            sm.switch
              [
                ( Parse_new_problem,
                  [
                    when_ (is_input '#')
                      [
                        bit_accum <-- new_bit_accum target_idx.value;
                        target_idx <-- target_idx.value +:. 1;
                      ];
                    when_ (is_input '.')
                      [ target_idx <-- target_idx.value +:. 1 ];
                    when_ (is_input ']')
                      [
                        sm.set_next Parse_button;
                        output.new_problem <-- vdd;
                        output.target <-- bit_accum.value;
                        clear;
                      ];
                  ] );
                ( Parse_button,
                  [
                    Util.try_parse_to_digit decimal data_in;
                    when_ (is_input ',')
                      [
                        bit_accum <-- new_bit_accum decimal.value;
                        decimal <--. 0;
                      ];
                    when_ (is_input ')')
                      [
                        output.new_button <-- vdd;
                        output.button <-- new_bit_accum decimal.value;
                        clear;
                      ];
                    when_ (is_input '{')
                      [
                        sm.set_next Wait_for_new_problem;
                        output.finish <-- vdd;
                        clear;
                      ];
                  ] );
                ( Wait_for_new_problem,
                  [ when_ (is_input '\n') [ sm.set_next Parse_new_problem ] ] );
              ];
          ];
      ];
    O.Of_always.value output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"parser" create
end

module GaussianElimination = struct
  module I = struct
    type 'a t = { clock : 'a; clear : 'a; commands : 'a Parser.O.t }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      (* New free variable *)
      free_variable : 'a;
      free_variables : 'a; [@bits max_dim]
      (* Fixed right hand side constants *)
      constant_rhs : 'a;
      constants_rhs : 'a; [@bits max_dim]
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Loading | Eliminate
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) ({ clock; clear; commands } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    let output = O.Of_always.wire zero in

    let%hw_var num_columns =
      Variable.reg spec ~width:(address_bits_for max_buttons)
    in
    let matrix =
      Array.init max_dim ~f:(fun _ ->
          Variable.reg spec ~width:(max_buttons + 1))
    in

    (* Stage Loading *)
    let write_column value =
      let values = split_lsb ~part_width:1 value in
      Array.map2_exn matrix (List.to_array values) ~f:(fun r v ->
          r <-- v @: msbs r.value)
      |> Array.to_list |> proc
    in

    let get_column =
      Array.map matrix ~f:(fun row -> msb row.value)
      |> Array.to_list |> concat_lsb
    in

    let pop_column =
      Array.map matrix ~f:(fun r -> r <-- lsbs r.value @: gnd)
      |> Array.to_list |> proc
    in

    (* Stage Processing *)
    let%hw_var done_mask = Variable.reg spec ~width:max_dim in

    let clear =
      proc
        [
          num_columns <--. 0;
          Array.map matrix ~f:(fun row -> row <--. 0) |> Array.to_list |> proc;
          done_mask <--. 0;
        ]
    in

    compile
      [
        sm.switch
          [
            ( Loading,
              [
                when_ commands.new_problem [ write_column commands.target ];
                when_ commands.new_button
                  [
                    write_column commands.button;
                    num_columns <-- num_columns.value +:. 1;
                  ];
                when_ commands.finish
                  [
                    (* We assume that new buttons and finish are exclusive *)
                    sm.set_next Eliminate;
                  ];
              ] );
            ( Eliminate,
              [
                (* If there are no columns left, return *)
                if_ (num_columns.value ==:. 0)
                  [
                    sm.set_next Loading;
                    output.constant_rhs <-- vdd;
                    output.constants_rhs <-- get_column;
                    clear;
                  ]
                  [
                    (* Find the pivot index *)
                    (let pivot_idx =
                       priority_select
                         (split_lsb ~part_width:1
                            (get_column &: ( ~: ) done_mask.value)
                         |> List.mapi ~f:(fun idx valid ->
                             {
                               With_valid.valid;
                               value =
                                 of_int idx ~width:(address_bits_for max_dim);
                             }))
                     in
                     if_ pivot_idx.valid
                       [
                         done_mask
                         <-- (done_mask.value
                             |: uresize
                                  (binary_to_onehot pivot_idx.value)
                                  max_dim);
                         (let pivot_row =
                            mux pivot_idx.value
                              (Array.map matrix ~f:(fun row -> row.value)
                              |> Array.to_list)
                          in
                          Array.mapi matrix ~f:(fun idx row ->
                              if_
                                (pivot_idx.value <>:. idx &: msb row.value)
                                [ row <-- lsbs (row.value ^: pivot_row) @: gnd ]
                                [ row <-- lsbs row.value @: gnd ])
                          |> Array.to_list |> proc);
                         num_columns <-- num_columns.value -:. 1;
                       ]
                       [
                         output.free_variable <-- vdd;
                         output.free_variables <-- get_column;
                         pop_column;
                         num_columns <-- num_columns.value -:. 1;
                       ]);
                  ];
              ] );
          ];
      ];
    O.Of_always.value output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"ge" create
end

module Solver = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      commands : 'a GaussianElimination.O.t;
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { valid : 'a; output : 'a [@bits output_bits] }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Loading | Compute_clicks | Reduce_clicks
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) ({ clock; clear; commands } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    let output = O.Of_always.wire zero in

    let free_variables_coefs =
      Array.init max_dim ~f:(fun _ ->
          Variable.reg spec ~width:max_free_variables)
    in
    let constant_coefs = Variable.reg spec ~width:max_dim in

    let clicks =
      Array.init (Int.shift_left 1 max_free_variables) ~f:(fun _ ->
          Variable.reg
            (Reg_spec.override
               ~clear_to:(ones (address_bits_for max_buttons))
               spec)
            ~width:(address_bits_for max_buttons))
    in

    let add_column values =
      let values = split_lsb values ~part_width:1 |> List.to_array in
      Array.map2_exn values free_variables_coefs ~f:(fun v r ->
          r <-- lsbs r.value @: v)
      |> Array.to_list |> proc
    in

    let clear =
      proc
        [
          Array.map free_variables_coefs ~f:(fun row -> row <--. 0)
          |> Array.to_list |> proc;
          constant_coefs <--. 0;
          Array.map clicks ~f:(fun row -> row <-- ones (width row.value))
          |> Array.to_list |> proc;
        ]
    in

    compile
      [
        sm.switch
          [
            ( Loading,
              [
                when_ commands.free_variable
                  [ add_column commands.free_variables ];
                when_ commands.constant_rhs
                  [
                    sm.set_next Compute_clicks;
                    constant_coefs <-- commands.constants_rhs;
                  ];
              ] );
            ( Compute_clicks,
              [
                Array.mapi clicks ~f:(fun i r ->
                    let i_signal = of_int i ~width:max_free_variables in
                    let constant_coefs =
                      split_lsb constant_coefs.value ~part_width:1
                      |> List.to_array
                    in
                    let free_variables =
                      Array.map2_exn free_variables_coefs constant_coefs
                        ~f:(fun v c ->
                          let v = v.value &: i_signal in
                          let elems = c :: split_lsb v ~part_width:1 in
                          tree ~arity:2 ~f:(reduce ~f:( ^: )) elems)
                      |> Array.to_list
                    in
                    let presses =
                      popcount (i_signal @: concat_lsb free_variables)
                    in
                    r <-- presses)
                |> Array.to_list |> proc;
                sm.set_next Reduce_clicks;
              ] );
            ( Reduce_clicks,
              [
                (let min_clicks =
                   Array.map clicks ~f:(fun r -> r.value)
                   |> Array.to_list
                   |> tree ~arity:2
                        ~f:(reduce ~f:(fun a b -> mux2 (a <=: b) a b))
                 in
                 proc
                   [
                     sm.set_next Loading;
                     clear;
                     output.output <-- uresize min_clicks output_bits;
                     output.valid <-- vdd;
                   ]);
              ] );
          ];
      ];
    O.Of_always.value output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"solver" create
end

module O = struct
  type 'a t = { part1 : 'a [@bits output_bits] } [@@deriving hardcaml]
end

let create (scope : Scope.t) ({ clock; clear; _ } as input_signal : _ I.t) :
    _ O.t =
  let commands = Parser.hierarchical scope input_signal in
  let gaussian_elim =
    GaussianElimination.hierarchical scope { clock; clear; commands }
  in
  let solver =
    Solver.hierarchical scope { clock; clear; commands = gaussian_elim }
  in
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in
  let%hw_var part1 = Variable.reg spec ~width:output_bits in

  compile [ when_ solver.valid [ part1 <-- part1.value +: solver.output ] ];
  { part1 = part1.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day10" create
