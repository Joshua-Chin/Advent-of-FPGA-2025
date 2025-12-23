open! Base
open! Hardcaml
open! Signal

let elem_bits = 17 (* 5 Digits *)
let output_bits = 2 * elem_bits
let max_convex_points = 512

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
      new_point : 'a;
      x : 'a; [@bits elem_bits]
      y : 'a; [@bits elem_bits]
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Parse_x | Parse_y
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    let%hw_var decimal = Variable.reg spec ~width:elem_bits in
    let%hw_var x = Variable.reg spec ~width:elem_bits in
    let output = O.Of_always.wire zero in
    compile
      [
        when_ data_in_valid
          [
            Util.try_parse_to_digit decimal data_in;
            sm.switch
              [
                ( Parse_x,
                  [
                    when_
                      (data_in ==:. Char.to_int ',')
                      [
                        sm.set_next Parse_y; x <-- decimal.value; decimal <--. 0;
                      ];
                  ] );
                ( Parse_y,
                  [
                    when_
                      (data_in ==:. Char.to_int '\n')
                      [
                        sm.set_next Parse_x;
                        output.new_point <-- vdd;
                        output.x <-- x.value;
                        output.y <-- decimal.value;
                        x <--. 0;
                        decimal <--. 0;
                      ];
                  ] );
              ];
          ];
      ];
    O.Of_always.value output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"parser" create
end

module O = struct
  type 'a t = {
    part1 : 'a; [@bits output_bits]
  }
  [@@deriving hardcaml]
end

let area x1 y1 x2 y2 =
  let abs_diff a b = mux2 (a >: b) (a -: b) (b -: a) in
  (abs_diff x1 x2 +:. 1) *: (abs_diff y1 y2 +:. 1)

let create (scope : Scope.t) ({ clock; clear; _ } as raw_input : _ I.t) : _ O.t
    =
  let commands = Parser.hierarchical scope raw_input in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let points =
    Array.init max_convex_points ~f:(fun _ -> Parser.O.Of_always.reg spec)
  in
  let areas =
    Array.init max_convex_points ~f:(fun _ ->
        Variable.reg spec ~width:output_bits)
  in

  let max_area =
    tree
      (areas |> Array.map ~f:(fun x -> x.value) |> Array.to_list)
      ~arity:16
      ~f:(fun signals ->
        tree signals ~arity:2 ~f:(reduce ~f:(fun a b -> mux2 (a >: b) a b)))
  in
  let part1 =
    reg_fb spec ~width:output_bits ~f:(fun signal ->
        mux2 (max_area >: signal) max_area signal)
  in

  compile
    [
      when_ commands.new_point
        [
          (* Update areas *)
          Array.map2_exn areas points ~f:(fun a p ->
              when_ p.new_point.value
                [ a <-- area commands.x commands.y p.x.value p.y.value ])
          |> Array.to_list |> proc;
          (* Insert point *)
          Array.mapi points ~f:(fun i p ->
              let prev =
                if i = 0 then commands
                else Parser.O.Of_always.value points.(i - 1)
              in
              Parser.O.Of_always.assign p prev)
          |> Array.to_list |> proc;
        ];
    ];
  { part1 }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day09" create
