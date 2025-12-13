open! Base
open! Stdio
open! Hardcaml
open! Advent_of_fpga_2025

let create_sim () =
  let module Sim = Cyclesim.With_interface (Day01.I) (Day01.O) in
  let scope =
    Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
  in
  Sim.create (Day01.hierarchical scope)

let () =
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let send_char char =
    inputs.data_in := Bits.of_char char;
    inputs.data_in_valid := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.data_in_valid := Bits.gnd
  in

  let send_string string = String.iter string ~f:send_char in

  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;

  send_string "R800\n";

  print_endline (Int.to_string (Bits.to_int !(outputs.password)))
