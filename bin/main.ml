open! Base
open! Stdio
open! Hardcaml

open! Advent_of_fpga_2025


let create_sim () =
  let module Sim = Cyclesim.With_interface (Day01.I) (Day01.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  Sim.create (Day01.hierarchical scope)

let () =
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  print_endline (Bits.to_string !(outputs.password))