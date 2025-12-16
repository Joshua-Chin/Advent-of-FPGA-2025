open! Base
open! Hardcaml
open! Signal

let output_bits = 10 (* Up to 1000 solutions *)
let max_tiles = 8
let max_tile_rows = 3
let max_tile_cols = 3
let max_tile_size = max_tile_rows * max_tile_cols
let max_bits = 7 (* 0 to 99 *)

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
  type 'a t = {
    min_solution : 'a; [@bits output_bits]
    max_solution : 'a; [@bits output_bits]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Parsing_tile_newline
    | Parsing_tile
    | Parsing_row
    | Parsing_col
    | Parsing_counts_init
    | Parsing_counts
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let div_3 x =
  let m, s = Util.compute_multiply_shift 3 100 in
  let m_signal = of_int m ~width:(Util.bit_length m) in
  uresize (srl (x *: m_signal) s) (width x - 1)

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in

  (* Declare RAM *)
  let%hw_var write_address =
    Variable.wire ~default:(zero (address_bits_for max_tiles))
  in

  let%hw_var write_enable = Variable.wire ~default:gnd in
  let%hw_var write_data =
    Variable.wire ~default:(zero (Int.floor_log2 max_tile_size + 1))
  in
  let%hw_var read_address =
    Variable.wire ~default:(zero (address_bits_for max_tiles))
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
    Ram.create ~name:"tile_cells" ~collision_mode:Read_before_write
      ~size:max_tiles ~write_ports:[| write_port |] ~read_ports:[| read_port |]
      ()
  in
  let ram_out = q.(0) in

  (* Declare Registers *)

  (* Registers for parsing *)
  let%hw_var curr_tile_idx =
    Variable.reg spec ~width:(address_bits_for max_tiles)
  in
  let%hw_var curr_tile_cells =
    Variable.reg spec ~width:(Int.floor_log2 max_tile_size + 1)
  in

  let%hw_var rows = Variable.reg spec ~width:max_bits in
  let%hw_var cols = Variable.reg spec ~width:max_bits in
  let%hw_var curr_tile_count = Variable.reg spec ~width:max_bits in

  (* Registers for intermediate values *)
  let%hw_var area_blocks = Variable.reg spec ~width:(2 * max_bits) in
  let%hw_var area_cells = Variable.reg spec ~width:(2 * max_bits) in
  let%hw_var tile_blocks = Variable.reg spec ~width:(2 * max_bits) in
  let%hw_var tile_cells = Variable.reg spec ~width:(2 * max_bits) in

  let%hw_var problem_ready = Variable.reg spec ~width:1 in

  let%hw_var min_solution = Variable.reg spec ~width:output_bits in
  let%hw_var max_solution = Variable.reg spec ~width:output_bits in

  let is_input c = data_in ==:. Char.to_int c in

  let parse_for_tile =
    proc
      [
        when_ (is_input '#') [ curr_tile_cells <-- curr_tile_cells.value +:. 1 ];
        Util.try_parse_to_digit rows data_in;
      ]
  in

  let prepare_ram idx = proc [ read_enable <-- vdd; read_address <-- idx ] in

  let handle_count =
    let new_idx = curr_tile_idx.value +:. 1 in
    proc
      [
        tile_blocks
        <-- tile_blocks.value
            +: uresize curr_tile_count.value (width tile_blocks.value);
        tile_cells
        <-- tile_cells.value
            +: uresize
                 (curr_tile_cells.value *: curr_tile_count.value)
                 (width tile_cells.value);
        prepare_ram new_idx;
        curr_tile_idx <-- new_idx;
        curr_tile_count <--. 0;
      ]
  in

  compile
    [
      when_ data_in_valid
        [
          sm.switch
            [
              ( Parsing_tile_newline,
                [
                  write_enable <-- gnd;
                  if_ (is_input '\n')
                    [
                      write_enable <-- vdd;
                      write_data <-- curr_tile_cells.value;
                      write_address <-- curr_tile_idx.value;
                      curr_tile_idx <-- curr_tile_idx.value +:. 1;
                      curr_tile_cells <--. 0;
                      rows <--. 0;
                    ]
                    [ write_enable <-- gnd; sm.set_next Parsing_tile ];
                  parse_for_tile;
                ] );
              ( Parsing_tile,
                [
                  when_ (is_input '\n') [ sm.set_next Parsing_tile_newline ];
                  (* If the input is `x`, we just finished parsing a row *)
                  when_ (is_input 'x')
                    [
                      sm.set_next Parsing_col;
                      curr_tile_idx <--. 0;
                      curr_tile_cells <--. 0;
                    ];
                  parse_for_tile;
                ] );
              ( Parsing_row,
                [
                  when_ (is_input 'x') [ sm.set_next Parsing_col ];
                  Util.try_parse_to_digit rows data_in;
                ] );
              ( Parsing_col,
                [
                  when_ (is_input ' ')
                    [
                      sm.set_next Parsing_counts_init;
                      prepare_ram curr_tile_idx.value;
                      area_blocks
                      <-- uresize
                            (div_3 rows.value *: div_3 cols.value)
                            (width area_blocks.value);
                      area_cells
                      <-- uresize (rows.value *: cols.value)
                            (width area_blocks.value);
                      rows <--. 0;
                      cols <--. 0;
                    ];
                  Util.try_parse_to_digit cols data_in;
                ] );
              ( Parsing_counts_init,
                [
                  curr_tile_cells <-- ram_out;
                  sm.set_next Parsing_counts;
                  Util.try_parse_to_digit curr_tile_count data_in;
                ] );
              ( Parsing_counts,
                [
                  when_ (is_input '\n')
                    [
                      sm.set_next Parsing_row;
                      problem_ready <-- vdd;
                      handle_count;
                    ];
                  when_ (is_input ' ')
                    [ sm.set_next Parsing_counts_init; handle_count ];
                  Util.try_parse_to_digit curr_tile_count data_in;
                ] );
            ];
        ];
      when_ problem_ready.value
        [
          problem_ready <-- gnd;
          area_blocks <--. 0;
          area_cells <--. 0;
          tile_blocks <--. 0;
          tile_cells <--. 0;
          curr_tile_idx <--. 0;
          if_
            (area_blocks.value >=: tile_blocks.value)
            [
              min_solution <-- min_solution.value +:. 1;
              max_solution <-- max_solution.value +:. 1;
            ]
            [
              when_
                (tile_cells.value <=: area_cells.value)
                [ max_solution <-- max_solution.value +:. 1 ];
            ];
        ];
    ];

  { min_solution = min_solution.value; max_solution = max_solution.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day12" create
