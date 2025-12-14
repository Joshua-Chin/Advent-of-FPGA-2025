open! Base
open! Hardcaml
open! Signal

let output_bits = 16

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    finish : 'a;
    data_in : 'a; [@bits 8]
    data_in_valid : 'a;
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    part1 : 'a; [@bits output_bits]
    part1_valid : 'a;
    part2 : 'a; [@bits output_bits]
    part2_valid : 'a;
  }
  [@@deriving hardcaml]
end

module Config = struct
  type t = { rows : int; cols : int }
end

module States = struct
  type t = Accepting_inputs | Processing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create ~config:({ rows; cols } : Config.t) (scope : Scope.t)
    ({ clock; clear; finish; data_in; data_in_valid } : _ I.t) : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var grid = Variable.reg spec ~width:(rows * cols) in

  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part1_valid = Variable.reg spec ~width:1 in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let%hw_var part2_valid = Variable.reg spec ~width:1 in

  let processing_grid =
    mux2 (sm.is Accepting_inputs) (zero (rows * cols)) grid.value
  in

  let get_cell row col =
    if 0 <= row && row < rows && 0 <= col && col < cols then
      let idx = (row * cols) + col in
      Some processing_grid.:[idx, idx]
    else None
  in

  let get_cell_exn row col =
    if 0 <= row && row < rows && 0 <= col && col < cols then
      let idx = (row * cols) + col in
      processing_grid.:[idx, idx]
    else raise (Invalid_argument "Index is out of bounds.")
  in

  let get_neighbors row col =
    let neighbors =
      [
        get_cell (row - 1) (col - 1);
        get_cell (row - 1) col;
        get_cell (row - 1) (col + 1);
        get_cell row (col - 1);
        get_cell row (col + 1);
        get_cell (row + 1) (col - 1);
        get_cell (row + 1) col;
        get_cell (row + 1) (col + 1);
      ]
    in
    List.filter_map neighbors ~f:(fun x -> x)
  in

  let is_accessible row col =
    let count =
      get_neighbors row col
      |> tree ~arity:2 ~f:(fun xs ->
          let w = Signal.width (List.hd_exn xs) in
          reduce ~f:(fun a b -> uresize a (w + 1) +: uresize b (w + 1)) xs)
    in
    count <:. 4
  in

  let new_grid =
    List.concat_map (List.range 0 rows) ~f:(fun row ->
        List.map (List.range 0 cols) ~f:(fun col ->
            let cell = get_cell_exn row col in
            cell &: ~:(is_accessible row col)))
    |> concat_lsb
  in

  let changed = uresize (popcount (processing_grid ^: new_grid)) output_bits in

  compile
    [
      sm.switch
        [
          ( Accepting_inputs,
            [
              when_ data_in_valid
                [
                  when_
                    (data_in ==:. Char.to_int '@')
                    [ grid <-- vdd @: msbs grid.value ];
                  when_
                    (data_in ==:. Char.to_int '.')
                    [ grid <-- gnd @: msbs grid.value ];
                ];
              when_ finish [ sm.set_next Processing ];
            ] );
          ( Processing,
            [
              grid <-- new_grid;
              when_
                (part1_valid.value ==: gnd)
                [ part1 <-- changed; part1_valid <-- vdd ];
              part2 <-- part2.value +: changed;
              when_ (changed ==:. 0) [ part2_valid <-- vdd ];
            ] );
        ];
    ];
  {
    part1 = part1.value;
    part1_valid = part1_valid.value;
    part2 = part2.value;
    part2_valid = part2_valid.value;
  }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04"
    (create ~config:{ rows = 139; cols = 139 })
