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
  type 'a t = { rows : int; cols : int; initial_state : 'a Option.t }
end

module States = struct
  type t = Accepting_inputs | Processing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create ~config:({ rows; cols; initial_state } : _ Config.t)
    (scope : Scope.t) ({ clock; clear; finish; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var grid =
    match initial_state with
    | Some state ->
        Variable.reg
          (Reg_spec.override ~clear_to:state spec)
          ~width:(rows * cols)
    | None -> Variable.reg spec ~width:(rows * cols)
  in

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

  let pipelined_popcount xs =
    xs
    |> List.map ~f:(fun b -> (0, b))
    |> tree ~arity:(2 ** 4) ~f:(fun ys ->
        let depths, counts = List.unzip ys in
        let add_counts a b =
          let w = max (width a) (width b) + 1 in
          uresize a w +: uresize b w
        in
        let sum = tree counts ~arity:2 ~f:(reduce ~f:add_counts) in
        let sum_reg = Signal.reg spec sum in
        ( Option.value_exn (List.max_elt depths ~compare:Int.compare) + 1,
          sum_reg ))
  in

  let depth, changed =
    pipelined_popcount (split_lsb ~part_width:1 (processing_grid ^: new_grid))
  in
  let changed = uresize changed output_bits in
  let changed_valid = pipeline spec ~n:depth (sm.is Processing) in

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
                (part1_valid.value ==: gnd &: changed_valid)
                [ part1 <-- changed; part1_valid <-- vdd ];
              part2 <-- part2.value +: changed;
              when_ (changed_valid &: (changed ==:. 0)) [ part2_valid <-- vdd ];
            ] );
        ];
    ];
  {
    part1 = part1.value;
    part1_valid = part1_valid.value;
    part2 = part2.value;
    part2_valid = part2_valid.value;
  }

let hierarchical ~config scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04" (create ~config)
