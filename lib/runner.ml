open! Base
open! Stdio
open! Hardcaml

let load_input () =
  let argv = Sys.get_argv () in
  match Array.length argv with
  | 2 ->
      let filename = argv.(1) in
      In_channel.read_all filename
  | _ ->
      eprintf "Usage: %s <filename>\n" argv.(0);
      Stdlib.exit 1

module type AdventOfFPGASolution = sig
  module I : Interface.S
  module O : Interface.S

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Make (M : AdventOfFPGASolution) = struct
  let create_sim () =
    let module Sim = Cyclesim.With_interface (M.I) (M.O) in
    let scope =
      Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
    in
    Sim.create (M.hierarchical scope)

  let run ?(latency = 0) () =
    (* Load the input *)
    let input = load_input () in

    (* Create the simulator *)
    let sim = create_sim () in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in

    (* Retrieve input ports *)
    let input_ports =
      M.I.zip M.I.port_names inputs
      |> M.I.to_list
      |> Hashtbl.of_alist_exn (module String)
    in
    let data_in = Hashtbl.find_exn input_ports "data_in" in
    let data_in_valid = Hashtbl.find_exn input_ports "data_in_valid" in
    let clear = Hashtbl.find_exn input_ports "clear" in
    let finish = Hashtbl.find input_ports "finish" in

    (* Retrieve output ports *)
    let output_ports =
      M.O.zip M.O.port_names outputs
      |> M.O.to_list
      |> Hashtbl.of_alist_exn (module String)
    in
    let part1 = Hashtbl.find output_ports "part1" in
    let part1_valid = Hashtbl.find output_ports "part1_valid" in
    let part2 = Hashtbl.find output_ports "part2" in
    let part2_valid = Hashtbl.find output_ports "part2_valid" in

    (* Clear the the circuit *)
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;

    (* Feed the input into the circuit *)
    String.iter input ~f:(fun c ->
        data_in := Bits.of_char c;
        data_in_valid := Bits.vdd;
        Cyclesim.cycle sim;
        data_in_valid := Bits.gnd);
    (* Set finish, if it exists *)
    Option.iter finish ~f:(fun finish -> finish := Bits.vdd);

    (* Run until valid *)
    let cycle_idx = ref 0 in
    let part1_done = ref (Option.is_none part1) in
    let part2_done = ref (Option.is_none part2) in
    let check_part idx is_done value valid =
      if not !is_done then
        match valid with
        | Some valid ->
            if Bits.to_bool !valid then (
              is_done := true;
              printf "Part %d: %d\n" idx (Bits.to_int !(Option.value_exn value)))
        | None ->
            if !cycle_idx >= latency then (
              is_done := true;
              printf "Part %d: %d\n" idx (Bits.to_int !(Option.value_exn value)))
    in

    while (not !part1_done) || not !part2_done do
      check_part 1 part1_done part1 part1_valid;
      check_part 2 part2_done part2 part2_valid;
      Cyclesim.cycle sim;
      cycle_idx := !cycle_idx + 1
    done;

    printf "Latency: %d cycle(s)\n" !cycle_idx
end
