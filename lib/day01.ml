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
  type 'a t = {
    password : 'a [@bits output_bits];
    } [@@deriving hardcaml]
end

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
  let%hw_var password = Variable.reg spec ~width:output_bits in

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
              hundreds
              <-- uresize (hundreds.value *: of_int 10 ~width:4) output_bits
                  +: uresize tens.value output_bits;
              tens <-- ones.value;
              (let d = data_in -:. Char.to_int '0' in
               ones <-- uresize d digit_bits);
            ];

          (* Logic *)
          when_
            (data_in ==:. Char.to_int '\n')
            [
              (let rotation =
                 uresize (tens.value *: of_int 10 ~width:4) dial_bits
                 +: uresize ones.value dial_bits
               in
               let new_password = password.value +: hundreds.value in
               if_ is_right.value
                 [
                   (let new_dial = dial.value +: rotation in
                    if_ (new_dial >=:. 100)
                      [
                        dial <-- new_dial -:. 100;
                        password <-- new_password +:. 1;
                      ]
                      [ dial <-- new_dial; password <-- new_password ]);
                 ]
                 [
                   (let new_dial = dial.value -: rotation in
                    let shift =
                      uresize (new_dial ==:. 0) output_bits
                      -: uresize (dial.value ==:. 0) output_bits
                      +: uresize (new_dial ==:. -100) output_bits
                    in
                    if_ (new_dial <+. 0)
                      [
                        dial <-- new_dial +:. 100;
                        password <-- new_password +: shift +:. 1;
                      ]
                      [ dial <-- new_dial; password <-- new_password +: shift ]);
                 ]);
                 (* Clear parser input *)
                 hundreds <-- zero output_bits;
                 tens <-- zero digit_bits;
                 ones <-- zero digit_bits;
                 
            ];
        ];
    ];

  { password = password.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day_1" create
