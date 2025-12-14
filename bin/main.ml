open! Base
open! Stdio
open! Hardcaml
open Hardcaml_waveterm

open! Advent_of_fpga_2025

let load_input () =
    let argv = Sys.get_argv () in
    match Array.length argv with
    | 2 ->
        let filename = argv.(1) in
        In_channel.read_all filename
    | _ ->
        eprintf "Usage: %s <filename>\n" argv.(0);
        Stdlib.exit 1

let create_sim () =
  let module Sim = Cyclesim.With_interface (Day04.I) (Day04.O) in
  let scope =
    Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
  in
  let input = load_input () in
  let bits = String.to_array input
    |> Array.filter_map ~f:(fun c ->
      if Char.equal c '@' then Some Signal.vdd else if Char.equal c '.' then Some Signal.gnd else None) in
  let state = Signal.of_array bits in
  Sim.create (Day04.hierarchical scope ~config:{rows = 139; cols = 139; initial_state = Some(state)} )

let () =
  let sim = create_sim () in
  let _waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let send_char char =
    inputs.data_in := Bits.of_char char;
    inputs.data_in_valid := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.data_in_valid := Bits.gnd
  in

  let _send_string string = String.iter string ~f:send_char in

  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;


  inputs.finish := Bits.vdd;

  while not (Bits.to_bool !(outputs.part2_valid)) do
    Cyclesim.cycle sim;
  done;

  printf "Part 1: %d, Part 2: %d\n" (Bits.to_int !(outputs.part1)) (Bits.to_int !(outputs.part2));