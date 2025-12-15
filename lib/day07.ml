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
  let%hw_var write_address =
    Variable.wire ~default:(zero (address_bits_for max_cols))
  in
  let%hw_var write_enable = Variable.wire ~default:gnd in
  let%hw_var write_data = Variable.wire ~default:(zero (output_bits + 1)) in
  let%hw_var read_address =
    Variable.wire ~default:(zero (address_bits_for max_cols))
  in
  let%hw_var read_enable = Variable.wire ~default:gnd in
  let write_port =
    {
      Write_port.write_clock = clock;
      write_address = write_address.value;
      write_enable = write_enable.value;
      write_data = write_data.value;
    }
  in
  let read_port =
    {
      Read_port.read_clock = clock;
      read_address = read_address.value;
      read_enable = read_enable.value;
    }
  in
  let q =
    Ram.create ~collision_mode:Read_before_write ~size:max_cols
      ~write_ports:[| write_port |] ~read_ports:[| read_port |] ()
  in
  let ram_out = q.(0) in

  (* Declare registers *)
  let%hw_var part1 = Variable.reg spec ~width:output_bits in
  let%hw_var part2 = Variable.reg spec ~width:output_bits in
  let%hw_var row_sum = Variable.reg spec ~width:output_bits in

  (* Kernel *)
  let%hw_var left = Variable.reg spec ~width:output_bits in
  let%hw_var left_splitter = Variable.reg spec ~width:1 in
  let%hw_var center = Variable.reg spec ~width:output_bits in
  let%hw_var center_splitter = Variable.reg spec ~width:1 in
  let right = lsbs ram_out in
  let right_splitter = msb ram_out in
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
      write_enable <-- gnd;
      when_ data_in_valid
        [
          if_
            (data_in ==:. Char.to_int '\n')
            [
              curr_col <--. 0;
              (* Update Kernel *)
              left <--. 0;
              left_splitter <-- gnd;
              center <-- lsbs first_data.value;
              center_splitter <-- msb first_data.value;
              read_enable <-- vdd;
              read_address <--. 1;
              (* Reset part 2 *)
              part2 <-- row_sum.value;
              row_sum <--. 0;
            ]
            [
              (* Enable Writes *)
              write_enable <-- vdd;
              write_address <-- curr_col.value;
              (* Parse each possible character *)
              when_
                (data_in ==:. Char.to_int 'S')
                [ write_data <-- gnd @: one output_bits ];
              when_
                (data_in ==:. Char.to_int '^')
                [ write_data <-- vdd @: kernel_sum ];
              when_
                (data_in ==:. Char.to_int '.')
                [ write_data <-- gnd @: kernel_sum ];
              (* Rotate the kernel *)
              left <-- center.value;
              left_splitter <-- center_splitter.value;
              center <-- right;
              center_splitter <-- right_splitter;
              read_enable <-- vdd;
              read_address <-- curr_col.value +:. 2;
              curr_col <-- curr_col.value +:. 1;
              (* Track the first column *)
              when_ (curr_col.value ==:. 0) [ first_data <-- write_data.value ];
              (* Update the outputs *)
              when_
                (center.value <>:. 0 &: center_splitter.value)
                [ part1 <-- part1.value +:. 1 ];
              row_sum <-- row_sum.value +: lsbs write_data.value;
            ];
        ];
    ];
  { part1 = part1.value; part2 = part2.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day05" create
