open! Base
open! Hardcaml
open! Signal

let output_bits = 16
let rows = 150
let cols = 150

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
  type 'a t = { initial_state : 'a }
end

module States = struct
  type t = Accepting_inputs | Processing
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let rec pairwise = function
  | x :: (y :: _ as rest) -> (x, y) :: pairwise rest
  | _ -> []

let create ?(config : _ Config.t option)
    (scope : Scope.t) ({ clock; clear; finish; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let grid =
    match config with
    | Some state ->
        let state_array = to_array state.initial_state in
        Array.init rows ~f:(fun row ->
            Array.init cols ~f:(fun col ->
                let idx = (row * cols) + col in
                let clear_to =
                  if idx < Array.length state_array then state_array.(idx)
                  else gnd
                in
                Variable.reg (Reg_spec.override ~clear_to spec) ~width:1))
    | None ->
        Array.init rows ~f:(fun _ ->
            Array.init cols ~f:(fun _ -> Variable.reg spec ~width:1))
  in

  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part1_valid = Variable.reg spec ~width:1 in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let%hw_var part2_valid = Variable.reg spec ~width:1 in

  let shift_row new_value =
    Array.mapi grid.(0) ~f:(fun idx cell ->
        cell <-- if idx > 0 then grid.(0).(idx - 1).value else new_value)
    |> Array.to_list |> proc
  in

  let update_grid ~f =
    Array.mapi grid ~f:(fun i row ->
        Array.mapi row ~f:(fun j cell -> cell <-- f i j)
        |> Array.to_list |> proc)
    |> Array.to_list |> proc
  in

  let processing_grid =
    Array.map grid ~f:(fun row ->
        Array.map row ~f:(fun reg -> mux2 (sm.is Processing) reg.value gnd))
  in

  let get_cell row col =
    if 0 <= row && row < rows && 0 <= col && col < cols then
      Some processing_grid.(row).(col)
    else None
  in

  let get_cell_exn row col =
    match get_cell row col with
    | Some output -> output
    | None -> raise (Invalid_argument "Index is out of bounds.")
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
    Array.init rows ~f:(fun row ->
        Array.init cols ~f:(fun col ->
            let cell = get_cell_exn row col in
            cell &: ~:(is_accessible row col)))
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

  let changed_flatten =
    Array.map2_exn processing_grid new_grid ~f:(fun row new_row ->
        Array.map2_exn row new_row ~f:( ^: ))
    |> Array.map ~f:Array.to_list |> Array.to_list |> List.concat
  in

  let depth, changed_count = pipelined_popcount changed_flatten in
  let changed_count = uresize changed_count output_bits in
  let changed_valid = pipeline spec ~n:depth (sm.is Processing) in

  compile
    [
      sm.switch
        [
          ( Accepting_inputs,
            [
              when_ data_in_valid
                [
                  when_ (data_in ==:. Char.to_int '@') [ shift_row vdd ];
                  when_ (data_in ==:. Char.to_int '.') [ shift_row gnd ];
                  when_
                    (data_in ==:. Char.to_int '\n')
                    [
                      update_grid ~f:(fun row col ->
                          if row > 0 then grid.(row - 1).(col).value else gnd);
                    ];
                ];
              when_ finish [ sm.set_next Processing ];
            ] );
          ( Processing,
            [
              update_grid ~f:(fun row col -> new_grid.(row).(col));
              when_
                (part1_valid.value ==: gnd &: changed_valid)
                [ part1 <-- changed_count; part1_valid <-- vdd ];
              part2 <-- part2.value +: changed_count;
              when_
                (changed_valid &: (changed_count ==:. 0))
                [ part2_valid <-- vdd ];
            ] );
        ];
    ];
  {
    part1 = part1.value;
    part1_valid = part1_valid.value;
    part2 = part2.value;
    part2_valid = part2_valid.value;
  }

let hierarchical ?config scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04" (create ?config)
