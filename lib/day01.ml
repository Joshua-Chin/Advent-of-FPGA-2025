open! Base
open! Hardcaml
open! Signal

let digit_bits = 4 (* 0-9, inclusive*)
let dial_bits = 8 (* -99-99 OR 0-198, inclusive*)
let output_bits = 20

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

let rotate_dial dial direction magnitude =
  let limit = of_int ~width:dial_bits 100 in

  (* Logic if direction is positive *)
  let add_raw = dial +: magnitude in
  let add_wrapped = add_raw >=: limit in
  let add_next = mux2 add_wrapped (add_raw -: limit) add_raw in

  (* Logic if direction is negative *)
  let sub_raw = dial -: magnitude in
  let sub_wrapped = magnitude >: dial in
  let sub_zeros = sub_raw ==:. 0 ||: (sub_wrapped &: ~:(dial ==:. 0)) in
  let sub_next = mux2 sub_wrapped (sub_raw +: limit) sub_raw in

  let next_pos = mux2 direction add_next sub_next in
  let zeros = mux2 direction add_wrapped sub_zeros in
  (next_pos, zeros)

let decimal_shift current new_digit bits =
  uresize (current *: of_int 10 ~width:4) bits +: uresize new_digit bits

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  (* Registers for parsing *)
  let%hw_var is_right = Variable.reg spec ~width:1 in
  let%hw_var hundreds = Variable.reg spec ~width:output_bits in
  let%hw_var tens = Variable.reg spec ~width:digit_bits in
  let%hw_var ones = Variable.reg spec ~width:digit_bits in

  (* Registers for logic *)
  let%hw_var dial =
    let clear_to_50 =
      Reg_spec.override ~clear_to:(of_int 50 ~width:dial_bits) spec
    in
    Variable.reg clear_to_50 ~width:dial_bits
  in
  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in

  compile
    [
      when_ data_in_valid
        [
          (* Parsing *)
          when_ (data_in ==:. Char.to_int 'L') [ is_right <--. 0 ];
          when_ (data_in ==:. Char.to_int 'R') [ is_right <--. 1 ];
          when_
            (data_in >=:. Char.to_int '0' &: (data_in <=:. Char.to_int '9'))
            [
              hundreds <-- decimal_shift hundreds.value tens.value output_bits;
              tens <-- ones.value;
              ones <-- uresize (data_in -:. Char.to_int '0') digit_bits;
            ];
          (* Logic *)
          when_
            (data_in ==:. Char.to_int '\n')
            (let magnitude = decimal_shift tens.value ones.value dial_bits in
             let new_dial, hit_zero =
               rotate_dial dial.value is_right.value magnitude
             in
             [
               dial <-- new_dial;
               when_ (new_dial ==:. 0) [ part1 <-- part1.value +:. 1 ];
               part2
               <-- part2.value +: hundreds.value +: uresize hit_zero output_bits;
               (* Clear parser input *)
               hundreds <-- zero output_bits;
               tens <-- zero digit_bits;
               ones <-- zero digit_bits;
             ]);
        ];
    ];

  { part1 = part1.value; part2 = part2.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day_1" create
