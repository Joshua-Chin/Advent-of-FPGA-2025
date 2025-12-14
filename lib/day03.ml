open! Base
open! Hardcaml
open! Signal

let digit_bits = 4 (* 0-9, inclusive*)
let output_bits = 64
let max_digits = 12
let part1_digits = 2
let part2_digits = 12

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
  type 'a t = { part1 : 'a; [@bits output_bits] part2 : 'a [@bits output_bits] }
  [@@deriving hardcaml]
end

let decimal_shift current new_digit bits =
  uresize (current *: of_int 10 ~width:4) bits +: uresize new_digit bits

let rec pairwise = function
  | x :: (y :: _ as rest) -> (x, y) :: pairwise rest
  | _ -> []

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let%hw_var joltages = Variable.reg spec ~width:(max_digits * output_bits) in
  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let joltage_list = split_lsb ~part_width:output_bits joltages.value in

  compile
    [
      when_ data_in_valid
        [
          if_
            (data_in >:. Char.to_int '0' &: (data_in <=:. Char.to_int '9'))
            [
              (let digit = uresize (data_in -:. Char.to_int '0') digit_bits in
               let new_joltages =
                 List.map
                   (pairwise (gnd :: joltage_list))
                   ~f:(fun (prev, curr) ->
                     let candidate = decimal_shift prev digit output_bits in
                     mux2 (candidate >: curr) candidate curr)
                 |> concat_lsb
               in
               joltages <-- new_joltages);
            ]
            [
              part1
              <-- part1.value +: List.nth_exn joltage_list (part1_digits - 1);
              part2
              <-- part2.value +: List.nth_exn joltage_list (part2_digits - 1);
              joltages <-- zero (max_digits * output_bits);
            ];
        ];
    ];

  { part1 = part1.value; part2 = part2.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day03" create
