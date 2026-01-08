open! Base
open! Stdio
open! Hardcaml
open Hardcaml_waveterm
open! Advent_of_fpga_2025
module M = Advent_of_fpga_2025.Day10_2.Solver

let create_sim () =
  let module Sim = Cyclesim.With_interface (M.I) (M.O) in
  let scope =
    Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
  in
  Sim.create ~config:(Cyclesim.Config.trace `All_named) (M.hierarchical ~offset:1 ~stride_log2:1 scope)

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
  let outputs = Cyclesim.outputs ~clock_edge:Side.Before sim in

  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;

    let module NS = Day10_2.GaussianElimination.NullSpace in
  let null_space = NS.Of_bits.of_int 0 |> NS.map ~f:ref in
  let free = null_space.free_variables |> List.to_array in
  let of_free = Bits.of_int ~width:13 in
  free.(2) := of_free 1;
  free.(3) := of_free 8190;
  free.(4) := of_free 1;
  free.(5) := of_free 8190;
  free.(6) := of_free 1;

  let const = null_space.constants |> List.to_array in
  let of_const = Bits.of_int ~width:13 in
  const.(0) := of_const 5;
  const.(1) := of_const @@ 8191 - 2;
  const.(2) := of_const 7;
  const.(3) := of_const 8190;

  let upper = null_space.upper_bounds |> List.to_array in
  let of_upper = Bits.of_int ~width:9 in
  upper.(0) := of_upper 7;
  upper.(1) := of_upper 5;

  inputs.commands.null_space_valid := Bits.vdd;
  NS.iter2 inputs.commands.null_space null_space ~f:(fun a b -> a := !b);
  Cyclesim.cycle sim;
  inputs.commands.null_space_valid := Bits.gnd;

  (*
  let input = _load_input () in
    String.iter input ~f:(fun c ->
        inputs.data_in := Bits.of_char c;
        inputs.data_in_valid := Bits.vdd;
        Cyclesim.cycle sim;
        inputs.data_in_valid := Bits.gnd);
print_string input;
*)

  (*
  let module NS = Day10_2.GaussianElimination.NullSpace in
  let null_space = NS.Of_bits.of_int 0 |> NS.map ~f:ref in
  let free = null_space.free_variables |> List.to_array in
  let of_free = Bits.of_int ~width:13 in
  free.(2) := of_free 1;
  free.(3) := of_free 8190;
  free.(4) := of_free 1;
  free.(5) := of_free 8190;
  free.(6) := of_free 1;

  let const = null_space.constants |> List.to_array in
  let of_const = Bits.of_int ~width:13 in
  const.(0) := of_const 5;
  const.(1) := of_const @@ 8191 - 2;
  const.(2) := of_const 7;
  const.(3) := of_const 8190;

  let upper = null_space.upper_bounds |> List.to_array in
  let of_upper = Bits.of_int ~width:9 in
  upper.(0) := of_upper 7;
  upper.(1) := of_upper 5;

  inputs.commands.valid := Bits.vdd;
  NS.iter2 inputs.commands.null_space null_space ~f:(fun a b -> a := !b);
  Cyclesim.cycle sim;
  inputs.commands.valid := Bits.gnd;
  *)
  for _ = 0 to 50 do
    if true then
      print_s @@ M.O.sexp_of_t Bits.sexp_of_t @@ M.O.map ~f:( ! ) outputs;
    (* print_s @@ ((M.O.map ~f:(!) outputs).null_space |> List.chunks_of ~length:7 |> List.sexp_of_t (List.sexp_of_t Bits.sexp_of_t)); *)
    Cyclesim.cycle sim;
    print_endline "";
  done;

  Waveform.print _waves

(*   let test_case = Day10_2.Parser.TestCase.Of_bits.of_int 0 in
  let test_case = Day10_2.Parser.TestCase.map ~f:ref test_case in
  let target = List.to_array test_case.target in
  let of_target = Bits.of_int ~width:9 in
  target.(0) := of_target 3;
  target.(1) := of_target 5;
  target.(2) := of_target 4;
  target.(3) := of_target 7;

  let buttons = List.to_array test_case.buttons in
  let of_button = Bits.of_int ~width:(Day10_2.max_dim) in
  buttons.(0) := of_button 0b1000;
  buttons.(1) := of_button 0b1010;
  buttons.(2) := of_button 0b0100;
  buttons.(3) := of_button 0b1100;
  buttons.(4) := of_button 0b0101;
  buttons.(5) := of_button 0b0011;

  test_case.num_buttons := Bits.of_int 6 ~width:(Signal.address_bits_for Day10_2.max_buttons);

  let test_case = Day10_2.Parser.TestCase.map ~f:(!) test_case in
  Day10_2.Parser.TestCase.iter2 inputs.commands.test_case test_case ~f:(fun a b -> a := b);
  inputs.commands.test_case_valid := Bits.vdd;

  Cyclesim.cycle sim;
  inputs.commands.test_case_valid := Bits.gnd;
*)
