open! Base
open! Hardcaml
open! Signal

let num_bits = 10;

module I = struct
    type 'a t = 
        { clock: 'a
        ; clear: 'a
        ; data_in : 'a [@bits 8]
        ; data_in_valid : 'a
        }
    [@@deriving hardcaml]
end

module O = struct
    type 'a t = 
        {
            password: 'a [@bits num_bits]
        }
    [@@deriving hardcaml]
end


let create (scope: Scope.t) ({clock; clear; data_in; data_in_valid}: _ I.t): _ O.t
    =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in

    (* Registers for parsing *)
    let is_right = Variable.reg spec ~width:1 in
    let hundreds = Variable.reg spec ~width:num_bits in
    let tens = Variable.reg spec ~width:4 in 
    let ones = Variable.reg spec ~width:4 in

    (* Registers for logic *)
    let dial =
        let dial_width = 8 in
        let clear_to_50 = Reg_spec.override ~clear_to:(of_int 50 ~width:dial_width) spec in
        Variable.reg clear_to_50 ~width:dial_width
    in
    let password = Variable.reg spec ~width:num_bits in

    compile [
        when_ data_in_valid [
            (* Parsing *)
            when_ (data_in ==:. Char.to_int 'L') [
                is_right <--. 0;
            ];
            when_ (data_in ==:. Char.to_int 'R') [
                is_right <--. 1;
            ];
            when_ ((data_in >=:. Char.to_int '0' ) &: (data_in <=:. Char.to_int '9')) [
                (
                    let new_hundreds = hundreds.value *: (of_int 10 ~width:4) +: tens.value in
                    hundreds <-- uresize new_hundreds num_bits
                );
                tens <-- ones.value;
                (
                    let d = data_in -:. (Char.to_int '0') in
                    ones <-- uresize d 4
                );
            ];
            (* Logic *)
            when_ (data_in ==:. Char.to_int '\n') [
                let rotation = tens.value *: (of_int 10 ~width:4) +: ones.value in
                let new_password = password.value +: hundreds.value in
                if_ is_right.value [
                    let new_dial = dial.value +: rotation in
                    if_ (new_dial >=:. 100) [
                        dial <-- new_dial -:. 100;
                        password <-- new_password +:. 1;
                    ] [
                        dial <-- rotation;
                        password <-- new_password;
                    ]
                ] [
                    let new_dial = dial.value -: rotation in
                    let shift = (
                        uresize (new_dial ==:. 0) num_bits -:
                        uresize (dial.value ==:. 0) num_bits
                    ) in
                    if_ (new_dial <+. 0) [
                        dial <-- new_dial +:. 100;
                        password <-- new_password +: shift +:. 1;
                    ] [
                        dial <-- new_dial;
                        password <-- new_password +: shift;
                    ];
                ];
            ];
        ];
    ];

    { password = password.value }