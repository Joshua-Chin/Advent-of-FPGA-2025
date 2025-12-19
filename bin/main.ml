open! Base
open! Stdio
open! Hardcaml
open Hardcaml_waveterm
open! Advent_of_fpga_2025
module M = Day10.Solver

let create_sim () =
  let module Sim = Cyclesim.With_interface (M.I) (M.O) in
  let scope =
    Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
  in
  Sim.create ~config:(Cyclesim.Config.trace `All_named) (M.hierarchical scope)

let _load_input () =
  let argv = Sys.get_argv () in
  match Array.length argv with
  | 2 ->
      let filename = argv.(1) in
      In_channel.read_all filename
  | _ ->
      eprintf "Usage: %s <filename>\n" argv.(0);
      Stdlib.exit 1

let () =
  let sim = create_sim () in
  let _waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs ~clock_edge:Side.Before sim in

  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;

  (*
  let send_char char =
    inputs.data_in := Bits.of_char char;
    inputs.data_in_valid := Bits.vdd;
    Cyclesim.cycle sim;
  in

  let input = load_input () in
  String.iter input ~f:(fun c ->
      send_char c);

  inputs.data_in_valid := Bits.gnd;
  *)

  inputs.commands.free_variable := Bits.vdd;
  inputs.commands.free_variables := Bits.of_int ~width:10 0b0100101000;
  Cyclesim.cycle sim;

  inputs.commands.free_variable := Bits.vdd;
  inputs.commands.free_variables := Bits.of_int ~width:10 0b0001010110;
  Cyclesim.cycle sim;

  inputs.commands.free_variable := Bits.gnd;
  inputs.commands.constant_rhs := Bits.vdd;
  inputs.commands.constants_rhs := Bits.of_int ~width:10 0b1100010010;
  Cyclesim.cycle sim;

  inputs.commands.constant_rhs := Bits.gnd;

  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;

  Waveform.print _waves
