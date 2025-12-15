open! Base
open! Hardcaml
open! Signal

let output_bits = 50 (* Up to 15 digits *)
let max_ranges_plus_1 = 200 + 1

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    data_in : 'a; [@bits 8]
    data_in_valid : 'a;
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    part1 : 'a; [@bits output_bits]
    part2 : 'a; [@bits output_bits]
    part2_valid : 'a;
    ranges : 'a; [@bits 2 * output_bits * max_ranges_plus_1]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Parse_start | Parse_last | Parse_ingredient
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let try_parse_to_digit current new_digit =
  Always.(
    when_
      (new_digit >=:. Char.to_int '0' &: (new_digit <=:. Char.to_int '9'))
      [
        current
        <-- uresize (current.value *: of_int 10 ~width:4) (width current.value)
            +: uresize (new_digit -:. Char.to_int '0') (width current.value);
      ])

let rec pairwise = function
  | x :: (y :: _ as rest) -> (x, y) :: pairwise rest
  | _ -> []

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in

  (* Parsing registers *)
  let%hw_var start = Variable.reg spec ~width:output_bits in
  let%hw_var last = Variable.reg spec ~width:output_bits in
  let%hw_var ingredient = Variable.reg spec ~width:output_bits in

  let%hw_var ranges =
    let width = max_ranges_plus_1 * 2 * output_bits in
    let clear_to_max = Reg_spec.override ~clear_to:(ones width) spec in
    Variable.reg clear_to_max ~width
  in

  let range_list =
    split_msb ~part_width:(2 * output_bits) ranges.value
    |> List.map ~f:(fun pair ->
        let pair_list = split_msb ~part_width:output_bits pair in
        (List.nth_exn pair_list 0, List.nth_exn pair_list 1))
  in

  let insorted (start, last) =
    let input_packed = start @: (last +:. 1) in
    let ranges_packed =
      List.map range_list ~f:(fun (s, e) -> (s >: start, s @: e))
    in
    pairwise ((gnd, input_packed) :: ranges_packed)
    |> List.map
         ~f:(fun ((prev_greater, prev_packed), (curr_greater, curr_packed)) ->
           mux2 curr_greater
             (mux2 prev_greater prev_packed input_packed)
             curr_packed)
    |> concat_msb
  in

  let is_fresh ingredient =
    List.map range_list ~f:(fun (start, end_) ->
        start <=: ingredient &: (ingredient <: end_))
    |> tree ~arity:2 ~f:(reduce ~f:( ||: ))
  in

  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let%hw_var part2_valid = Variable.reg spec ~width:1 in

  compile
    [
      part2 <-- part1.value;
      part2_valid <-- gnd;
      when_ data_in_valid
        [
          sm.switch
            [
              ( Parse_start,
                [
                  when_
                    (data_in ==:. Char.to_int '-')
                    [ sm.set_next Parse_last ];
                  when_
                    (data_in ==:. Char.to_int '\n')
                    [ sm.set_next Parse_ingredient ];
                  try_parse_to_digit start data_in;
                ] );
              ( Parse_last,
                [
                  when_
                    (data_in ==:. Char.to_int '\n')
                    [
                      ranges <-- insorted (start.value, last.value);
                      start <-- zero (width start.value);
                      last <-- zero (width last.value);
                      sm.set_next Parse_start;
                    ];
                  try_parse_to_digit last data_in;
                ] );
              ( Parse_ingredient,
                [
                  when_
                    (data_in ==:. Char.to_int '\n')
                    [
                      when_
                        (is_fresh ingredient.value)
                        [ part1 <-- part1.value +:. 1 ];
                      ingredient <-- zero (width ingredient.value);
                    ];
                  try_parse_to_digit ingredient data_in;
                ] );
            ];
        ];
    ];
  { part1 = part1.value; part2 = part2.value; part2_valid = part2_valid.value; ranges = ranges.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day05" create
