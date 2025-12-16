open! Base
open! Hardcaml
open! Signal

let max_cols = 4000
let max_blocks = max_cols / 2
let max_rows = 4
let output_bits = 50 (* Up to 15 digits *)
let element_bits = 14
let accumulator_bits = min output_bits (max_rows * element_bits)

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

module Part1Parser = struct
  module O = struct
    type 'a t = {
      (* Add a new element to a block *)
      new_element : 'a;
      element : 'a; [@bits element_bits]
      (* The line is finished *)
      new_row : 'a;
      (* Add a new operator to a block *)
      new_op : 'a;
      is_multiply : 'a;
    }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    (* State *)
    let decimal = Variable.reg spec ~width:element_bits in
    let%hw_var is_gap = Variable.reg spec ~width:1 in
    let is_ops = Variable.reg spec ~width:1 in
    (* Output *)
    let output = O.Of_always.wire zero in
    (* Helper actions *)
    let reset = proc [ decimal <--. 0; is_gap <-- gnd ] in

    let is_input c = data_in ==:. Char.to_int c in

    let emit_element =
      proc
        [ output.new_element <-- vdd; output.element <-- decimal.value; reset ]
    in

    let emit_op is_multiply =
      proc
        [
          output.new_op <-- vdd;
          output.is_multiply <-- is_multiply;
          is_ops <-- vdd;
        ]
    in

    compile
      [
        when_ data_in_valid
          [
            when_ (( ~: ) is_ops.value)
              [
                Util.try_parse_to_digit decimal data_in;
                if_
                  (is_input ' ' ||: is_input '\n')
                  [
                    when_ (( ~: ) is_gap.value) [ emit_element ];
                    when_ (is_input '\n') [ output.new_row <-- vdd ];
                    is_gap <-- vdd;
                  ]
                  [ is_gap <-- gnd ];
              ];
            when_ (data_in ==:. Char.to_int '*') [ emit_op vdd ];
            when_ (data_in ==:. Char.to_int '+') [ emit_op gnd ];
          ];
      ];
    O.Of_always.value output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"part1_parser" create
end

let create (scope : Scope.t) ({ clock; clear; _ } as input : _ I.t) : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let commands = Part1Parser.hierarchical scope input in
  (* Declare RAM *)
  let module SDPR = Util.SimpleDualPortRam (struct
    let size = max_blocks
    let item_width = accumulator_bits
  end) in
  let ram_sum = SDPR.I.Of_always.wire zero in
  let ram_sum_out = SDPR.create scope ~clock (SDPR.I.Of_always.value ram_sum) in
  let ram_prod = SDPR.I.Of_always.wire zero in
  let ram_prod_out =
    SDPR.create scope ~clock (SDPR.I.Of_always.value ram_prod)
  in
  (* Declare Register *)
  let%hw_var col_idx = Variable.reg spec ~width:(address_bits_for max_blocks) in
  let%hw_var should_read_ram = Variable.reg spec ~width:1 in
  let%hw_var curr_sum = Variable.reg spec ~width:accumulator_bits in
  let%hw_var curr_prod = Variable.reg (Reg_spec.override spec ~clear_to:(one accumulator_bits)) ~width:accumulator_bits in
  let%hw_var part1 = Variable.reg spec ~width:output_bits in

  let set_col_idx new_col_idx =
    proc
      [
        col_idx <-- new_col_idx;
        should_read_ram <-- vdd;
        ram_prod.read_enable <-- vdd;
        ram_prod.read_address <-- new_col_idx;
        ram_sum.read_enable <-- vdd;
        ram_sum.read_address <-- new_col_idx;
      ]
  in

  let update_sum =
    proc
      [
        ram_sum.write_enable <-- vdd;
        ram_sum.write_address <-- col_idx.value;
        ram_sum.write_data
        <-- curr_sum.value +: uresize commands.element accumulator_bits;
      ]
  in
  let update_prod =
    proc
      [
        ram_prod.write_enable <-- vdd;
        ram_prod.write_address <-- col_idx.value;
        ram_prod.write_data
        <-- uresize
              (uresize curr_prod.value (accumulator_bits - element_bits)
              *: commands.element)
              accumulator_bits;
      ]
  in

  compile
    [
      when_ should_read_ram.value
        [
          should_read_ram <-- gnd;
          curr_sum <-- ram_sum_out;
          curr_prod
          <-- mux2 (ram_prod_out ==:. 0)
                (one (width curr_prod.value))
                ram_prod_out;
        ];
      when_ commands.new_element
        [ update_sum; update_prod; set_col_idx (col_idx.value +:. 1) ];
      when_ commands.new_row [ set_col_idx (zero (width col_idx.value)) ];
      when_ commands.new_op
        [
          part1
          <-- part1.value
              +: uresize
                   (mux2 commands.is_multiply
                      (mux2 should_read_ram.value ram_prod_out curr_prod.value)
                      (mux2 should_read_ram.value ram_sum_out curr_sum.value))
                   (width part1.value);
          set_col_idx (col_idx.value +:. 1);
        ];
    ];
  { part1 = part1.value; part2 = zero output_bits }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day06" create
