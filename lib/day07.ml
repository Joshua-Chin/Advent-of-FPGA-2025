open! Base
open! Hardcaml
open! Signal

let output_bits = 50 (* Up to 15 digits *)
let max_cols = 256

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

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  (* Declare RAM *)
  let module SDPR = Util.SimpleDualPortRam (struct
    let size = max_cols
    let item_width = output_bits + 1
  end) in
  let ram = SDPR.I.Of_always.wire zero in
  let ram_out = SDPR.create scope ~clock (SDPR.I.Of_always.value ram) in
  let%hw_var should_read_ram = Variable.reg spec ~width:1 in

  (* Declare registers *)
  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let%hw_var row_sum = Variable.reg spec ~width:output_bits in

  (* Kernel *)
  let%hw_var left = Variable.reg spec ~width:output_bits in
  let%hw_var left_splitter = Variable.reg spec ~width:1 in
  let%hw_var center = Variable.reg spec ~width:output_bits in
  let%hw_var center_splitter = Variable.reg spec ~width:1 in
  let%hw_var right_cache = Variable.reg spec ~width:output_bits in
  let%hw_var right_splitter_cache = Variable.reg spec ~width:1 in

  let right = mux2 should_read_ram.value (lsbs ram_out) right_cache.value in
  let right_splitter =
    mux2 should_read_ram.value (msb ram_out) right_splitter_cache.value
  in
  let kernel_sum =
    let z = zero output_bits in
    mux2 left_splitter.value left.value z
    +: mux2 center_splitter.value z center.value
    +: mux2 right_splitter right z
  in

  let%hw_var first_data = Variable.reg spec ~width:(output_bits + 1) in

  let%hw_var curr_col = Variable.reg spec ~width:(address_bits_for max_cols) in

  compile
    [
      when_ data_in_valid
        [
          when_ should_read_ram.value
            [
              should_read_ram <-- gnd;
              right_cache <-- right;
              right_splitter_cache <-- right_splitter;
            ];
          if_
            (data_in ==:. Char.to_int '\n')
            [
              curr_col <--. 0;
              (* Update Kernel *)
              left <--. 0;
              left_splitter <-- gnd;
              center <-- lsbs first_data.value;
              center_splitter <-- msb first_data.value;
              should_read_ram <-- vdd;
              ram.read_enable <-- vdd;
              ram.read_address <--. 1;
              (* Reset part 2 *)
              part2 <-- row_sum.value;
              row_sum <--. 0;
            ]
            [
              (* Enable Writes *)
              ram.write_enable <-- vdd;
              ram.write_address <-- curr_col.value;
              (* Parse each possible character *)
              when_
                (data_in ==:. Char.to_int 'S')
                [ ram.write_data <-- gnd @: one output_bits ];
              when_
                (data_in ==:. Char.to_int '^')
                [ ram.write_data <-- vdd @: kernel_sum ];
              when_
                (data_in ==:. Char.to_int '.')
                [ ram.write_data <-- gnd @: kernel_sum ];
              (* Rotate the kernel *)
              left <-- center.value;
              left_splitter <-- center_splitter.value;
              center <-- right;
              center_splitter <-- right_splitter;
              should_read_ram <-- vdd;
              ram.read_enable <-- vdd;
              ram.read_address <-- curr_col.value +:. 2;
              curr_col <-- curr_col.value +:. 1;
              (* Track the first column *)
              when_ (curr_col.value ==:. 0)
                [ first_data <-- ram.write_data.value ];
              (* Update the outputs *)
              when_
                (center.value <>:. 0 &: center_splitter.value)
                [ part1 <-- part1.value +:. 1 ];
              row_sum <-- row_sum.value +: lsbs ram.write_data.value;
            ];
        ];
    ];
  { part1 = part1.value; part2 = part2.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day07" create
