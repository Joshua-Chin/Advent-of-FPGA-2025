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
    let high = (10 ** digits) - 1 in
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
          [
            range_lo
            <-- mux2 (start >=:. low) (uresize start bits)
                  (of_int low ~width:bits);
            range_hi
            <-- mux2 (last <=:. high) (uresize last bits)
                  (of_int high ~width:bits);
            valid1 <-- vdd;
          ];
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
  type 'a t = { part1 : 'a; [@bits output_bits] part2 : 'a [@bits output_bits] }
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

let mobius n =
  if n = 1 then 1
  else
    let rec f divisor n curr =
      if divisor * divisor > n then if n > 1 then -curr else curr
      else if n % divisor = 0 then
        let new_n = n / divisor in
        if new_n % divisor = 0 then 0 else f (divisor + 1) new_n (-curr)
      else f (divisor + 1) n curr
    in
    f 2 n 1

let precompute_terms_for_digit digit =
  List.range ~stop:`inclusive 2 digit
  |> List.filter ~f:(fun repeats -> digit % repeats = 0)
  |> List.filter_map ~f:(fun repeats ->
      let m = mobius repeats in
      if m <> 0 then Some (-m, repeats) else None)

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var start = Variable.reg spec ~width:range_bits in
  let%hw_var last = Variable.reg spec ~width:range_bits in
  let%hw_var in_valid = Variable.reg spec ~width:1 in
  let part1_solver_outputs, part2_solver_outputs =
    List.range 2 max_digits ~stop:`inclusive
    |> List.concat_map ~f:(fun digits ->
        precompute_terms_for_digit digits |> List.map ~f:(fun x -> (digits, x)))
    |> List.fold
         ~init:([], ([], []))
         ~f:(fun (p1, (p2p, p2n)) (digits, (coef, repeats)) ->
           let block_size = digits / repeats in
           let solver =
             FixedPatternSolver.hierarchical scope
               ~config:{ block_size; repeats }
           in
           printf "coef: %d, digits: %d, block size: %d, repeats: %d\n" coef
             digits block_size repeats;
           let raw_output =
             solver
               {
                 clock;
                 clear;
                 in_valid = in_valid.value;
                 start = start.value;
                 last = last.value;
               }
           in
           let output =
             mux2 raw_output.out_valid raw_output.sum (zero output_bits)
           in
           ( (if repeats = 2 then output :: p1 else p1),
             if coef = 1 then (output :: p2p, p2n) else (p2p, output :: p2n) ))
  in
  let part1_sum = tree ~arity:2 ~f:(reduce ~f:( +: )) part1_solver_outputs in

  let part2_sum =
    let p2p, p2n = part2_solver_outputs in
    let pos = tree ~arity:2 ~f:(reduce ~f:( +: )) p2p in
    let neg = tree ~arity:2 ~f:(reduce ~f:( +: )) p2n in
    pos -: neg
  in

  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  compile
    [
      in_valid <-- gnd;
      part1 <-- part1.value +: part1_sum;
      part2 <-- part2.value +: part2_sum;
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
  { part1 = part1.value; part2 = part2.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day02" create
