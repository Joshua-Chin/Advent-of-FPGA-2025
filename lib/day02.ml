open! Base
open! Stdio
open! Hardcaml
open! Signal

let digit_bits = 4 (* 0-9, inclusive*)
let range_bits = 36 (* Up to 10 digits *)
let output_bits = 64
let max_digits = 10

(* Solver for a fixed block size and number of repeats *)
module FixedPatternSolver = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      in_valid : 'a;
      start : 'a; [@bits range_bits]
      last : 'a; [@bits range_bits]
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out_valid : 'a; sum : 'a [@bits output_bits] }
    [@@deriving hardcaml]
  end

  module Config = struct
    type t = { block_size : int; repeats : int }
  end

  let bit_length x = Int.floor_log2 x + 1

  (* Algorithm from Hacker's Delight *)
  let compute_multiply_shift divisor upper_bounds =
    let w = bit_length upper_bounds in
    let nc = ((2 ** w) / divisor * divisor) - 1 in
    let p = ref w in
    while not (2 ** !p > nc * (divisor - ((2 ** !p) % divisor))) do
      p := !p + 1
    done;
    let m = ((2 ** !p) + divisor - ((2 ** !p) % divisor)) / divisor in
    (m, !p)

  let create ~config:({ block_size; repeats } : Config.t) (scope : Scope.t)
      ({ clock; clear; in_valid; start; last } : _ I.t) : _ O.t =
    ignore scope;

    (* Precompute constants *)
    let digits = block_size * repeats in
    let low = 10 ** (digits - 1) in
    let high = (10 ** (digits)) - 1 in
    let bits = bit_length high in
    let base = ((10 ** digits) - 1) / ((10 ** block_size) - 1) in
    let inv_base_m, inv_base_s = compute_multiply_shift base high in

    (* Declare registers *)
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let divide_precomputed x =
      let m_signal = of_int inv_base_m ~width:(bit_length inv_base_m) in
      let divided = srl (x *: m_signal) inv_base_s in
      uresize divided bits
    in

    (* Stage 1: Compute range *)
    let%hw_var range_lo = Variable.reg spec ~width:bits in
    let%hw_var range_hi = Variable.reg spec ~width:bits in
    let%hw_var valid1 = Variable.reg spec ~width:1 in

    (* Stage 2: Compute low and high multiples *)
    let%hw_var mult_lo = Variable.reg spec ~width:bits in
    let%hw_var mult_hi = Variable.reg spec ~width:bits in
    let%hw_var valid2 = Variable.reg spec ~width:1 in

    (* Stage 3: Compute count and twice mean *)
    let%hw_var range_count = Variable.reg spec ~width:bits in
    let%hw_var range_mean_2 = Variable.reg spec ~width:(bits + 1) in
    let%hw_var valid3 = Variable.reg spec ~width:1 in

    (* Stage 4: Compute sum over range *)
    let%hw_var range_sum = Variable.reg spec ~width:output_bits in
    let%hw_var valid4 = Variable.reg spec ~width:1 in

    (* Stage 5: Multiply sum over range with the base *)
    let%hw_var output = Variable.reg spec ~width:output_bits in
    let%hw_var valid5 = Variable.reg spec ~width:1 in

    compile
      [
        valid1 <-- gnd;
        valid2 <-- gnd;
        valid3 <-- gnd;
        valid4 <-- gnd;
        valid5 <-- gnd;
        (* Stage 1 *)
        when_
          (in_valid &: (last >=:. low) &: (start <=:. high))
          (let start_clamp = uresize start bits in
           let last_clamp = uresize last bits in
           [
             range_lo
             <-- mux2 (start_clamp >=:. low) start_clamp
                   (of_int low ~width:bits);
             range_hi
             <-- mux2 (last_clamp <=:. high) last_clamp
                   (of_int high ~width:bits);
             valid1 <-- vdd;
           ]);
        (* Stage 2 *)
        when_ valid1.value
          [
            mult_lo <-- divide_precomputed (range_lo.value -:. 1) +:. 1;
            mult_hi <-- divide_precomputed range_hi.value;
            valid2 <-- vdd;
          ];
        (* Stage 3 *)
        when_ valid2.value
          [
            range_count <-- mult_hi.value -: mult_lo.value +:. 1;
            range_mean_2
            <-- uresize mult_lo.value (bits + 1)
                +: uresize mult_hi.value (bits + 1);
            valid3 <-- vdd;
          ];
        (* Stage 4 *)
        when_ valid3.value
          [
            range_sum
            <-- uresize
                  (srl (range_count.value *: range_mean_2.value) 1)
                  output_bits;
            valid4 <-- vdd;
          ];
        (* Stage 5 *)
        when_ valid4.value
          [
            output
            <-- uresize
                  (range_sum.value *: of_int base ~width:(bit_length base))
                  output_bits;
            valid5 <-- vdd;
          ];
      ];
    { out_valid = valid5.value; sum = output.value }

  let hierarchical ~config scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"fixed_pattern_solver" (create ~config)
end

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
    valid : 'a;
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Start | Parse_start | Parse_last
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let is_digit x = x >=:. Char.to_int '0' &: (x <=:. Char.to_int '9')
let parse_digit s = uresize (s -:. Char.to_int '0') digit_bits

let decimal_shift current new_digit bits =
  uresize (current *: of_int 10 ~width:4) bits +: uresize new_digit bits

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var start = Variable.reg spec ~width:range_bits in
  let%hw_var last = Variable.reg spec ~width:range_bits in
  let%hw_var in_valid = Variable.reg spec ~width:1 in

  let solver_outputs =
    List.range 2 max_digits ~stop:`inclusive ~stride:2
    |> List.map ~f:(fun digits ->
        let repeats = 2 in
        let block_size = digits / repeats in
        let solver =
          FixedPatternSolver.hierarchical scope ~config:{ block_size; repeats }
        in
        solver
          {
            clock;
            clear;
            in_valid = in_valid.value;
            start = start.value;
            last = last.value;
          })
  in

  let output_sum =
    List.map solver_outputs ~f:(fun { out_valid; sum } ->
        mux2 out_valid sum (zero output_bits))
    |> tree ~arity:2 ~f:(reduce ~f:( +: ))
  in

  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let%hw_var out_valid = Variable.reg spec ~width:1 in
  compile
    [
      in_valid <-- gnd;
      part1 <-- part1.value +: output_sum;
      part2 <-- part2.value +: output_sum;
      out_valid <-- vdd;
      sm.switch
        [
          ( Start,
            [
              when_
                (data_in_valid &: is_digit data_in)
                [
                  start <-- uresize (parse_digit data_in) range_bits;
                  last <-- zero range_bits;
                  sm.set_next Parse_start;
                ];
            ] );
          ( Parse_start,
            [
              when_ data_in_valid
                [
                  when_ (is_digit data_in)
                    [
                      start
                      <-- decimal_shift start.value (parse_digit data_in)
                            range_bits;
                    ];
                  when_
                    (data_in ==:. Char.to_int '-')
                    [ sm.set_next Parse_last ];
                ];
            ] );
          ( Parse_last,
            [
              when_ data_in_valid
                [
                  when_ (is_digit data_in)
                    [
                      last
                      <-- decimal_shift last.value (parse_digit data_in)
                            range_bits;
                    ];
                  when_
                    (data_in ==:. Char.to_int ','
                    |: (data_in ==:. Char.to_int '\n'))
                    [ in_valid <-- vdd; sm.set_next Start ];
                ];
            ] );
        ];
    ];
  { part1 = part1.value; part2 = part2.value; valid = out_valid.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day02" create
