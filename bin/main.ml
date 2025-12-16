open! Base
open! Stdio
open! Hardcaml
open Hardcaml_waveterm
open! Advent_of_fpga_2025
module M = Day12

let create_sim () =
  let module Sim = Cyclesim.With_interface (M.I) (M.O) in
  let scope =
    Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
  in
  Sim.create ~config:(Cyclesim.Config.trace `All_named) (M.hierarchical scope)

let load_input () =
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
  let waves, sim = Waveform.create sim in
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

  let input = load_input () in
  send_string input;

  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;

  printf "Part 1: %d, Part 2: %d\n"
    (Bits.to_int !(outputs.min_solution))
    (Bits.to_int !(outputs.max_solution));

  Waveform.print waves ~display_height:100 ~display_width:200 ~start_cycle:120