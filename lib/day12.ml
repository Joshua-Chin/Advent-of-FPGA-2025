open! Base
open! Hardcaml
open! Signal

let output_bits = 10 (* Up to 1000 solutions *)
let max_tiles = 8
let tile_size_bits = 4 (* 0 to 9 *)
let dim_bits = 7
let accumulator_bits = 14

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

module TileParser = struct
  module O = struct
    type 'a t = {
      tile_sizes : 'a list; [@length max_tiles] [@bits tile_size_bits]
    }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let%hw_var current_idx =
      Variable.reg spec ~width:(address_bits_for max_tiles)
    in
    let%hw_var finished = Variable.reg spec ~width:1 in
    let%hw_var prev_newline = Variable.reg spec ~width:1 in
    let tile_sizes =
      List.init max_tiles ~f:(fun _ -> Variable.reg spec ~width:tile_size_bits)
    in

    compile
      [
        when_
          (data_in_valid &: ( ~: ) finished.value)
          [
            when_ (data_in ==:. Char.to_int 'x') [ finished <-- vdd ];
            when_
              (data_in ==:. Char.to_int '#')
              [
                List.mapi tile_sizes ~f:(fun idx r ->
                    when_ (current_idx.value ==:. idx) [ r <-- r.value +:. 1 ])
                |> proc;
              ];
            if_
              (data_in ==:. Char.to_int '\n')
              [
                when_ prev_newline.value
                  [ current_idx <-- current_idx.value +:. 1 ];
                prev_newline <-- vdd;
              ]
              [ prev_newline <-- gnd ];
          ];
      ];
    { tile_sizes = List.map tile_sizes ~f:(fun r -> r.value) }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"tile_parser" create
end

module ProblemParser = struct
  module O = struct
    type 'a t = {
      (* Command: New Problem *)
      new_grid : 'a;
      grid_rows : 'a; [@bits dim_bits]
      grid_cols : 'a; [@bits dim_bits]
      (* Command: Add Tile Requirement *)
      add_tile : 'a;
      tile_idx : 'a; [@bits address_bits_for max_tiles]
      tile_count : 'a; [@bits dim_bits]
      (* Command: Compute Solution *)
      finish_problem : 'a;
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Parse_row | Parse_col | Parse_counts
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in

    (* Output flags *)
    let%hw_var new_grid = Variable.wire ~default:gnd in
    let%hw_var add_tile = Variable.wire ~default:gnd in
    let%hw_var finish_problem = Variable.wire ~default:gnd in
    (* New Grid *)
    let%hw_var grid_rows = Variable.reg spec ~width:dim_bits in
    let%hw_var grid_cols = Variable.wire ~default:(zero dim_bits) in
    (* New Tile *)
    let%hw_var tile_idx =
      Variable.reg spec ~width:(address_bits_for max_tiles)
    in
    let%hw_var tile_count = Variable.wire ~default:(zero dim_bits) in
    (* Digit Parsing *)
    let%hw_var decimal = Variable.reg spec ~width:dim_bits in

    let reset = proc [ grid_rows <--. 0; tile_idx <--. 0; decimal <--. 0 ] in

    let is_input c = data_in ==:. Char.to_int c in

    compile
      [
        when_ data_in_valid
          [
            Util.try_parse_to_digit decimal data_in;
            sm.switch
              [
                ( Parse_row,
                  [
                    when_ (is_input 'x')
                      [
                        grid_rows <-- decimal.value;
                        decimal <--. 0;
                        sm.set_next Parse_col;
                      ];
                    (* We may have attempted to parse row that is not a problems *)
                    when_ (is_input '\n') [ reset ];
                  ] );
                ( Parse_col,
                  [
                    when_ (is_input ' ')
                      [
                        new_grid <-- vdd;
                        grid_cols <-- decimal.value;
                        decimal <--. 0;
                        sm.set_next Parse_counts;
                      ];
                  ] );
                ( Parse_counts,
                  [
                    when_ (is_input ' ')
                      [
                        add_tile <-- vdd;
                        tile_count <-- decimal.value;
                        decimal <--. 0;
                        tile_idx <-- tile_idx.value +:. 1;
                      ];
                    when_ (is_input '\n')
                      [
                        add_tile <-- vdd;
                        tile_count <-- decimal.value;
                        finish_problem <-- vdd;
                        reset;
                        sm.set_next Parse_row;
                      ];
                  ] );
              ];
          ];
      ];
    {
      (* New grid *)
      new_grid = new_grid.value;
      grid_rows = grid_rows.value;
      grid_cols = grid_cols.value;
      (* New tile *)
      add_tile = add_tile.value;
      tile_idx = tile_idx.value;
      tile_count = tile_count.value;
      (* Finished *)
      finish_problem = finish_problem.value;
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"problem_parser" create
end

module Solver = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      commands : 'a ProblemParser.O.t;
      tile_sizes : 'a TileParser.O.t;
    }
    [@@deriving hardcaml]
  end

  let div_3 x =
    let m, s = Util.compute_multiply_shift 3 100 in
    let m_signal = of_int m ~width:(Util.bit_length m) in
    uresize (srl (x *: m_signal) s) (width x - 1)

  let create (scope : Scope.t) ({ clock; clear; commands; tile_sizes } : _ I.t)
      : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    (* Declare Registers *)
    let%hw_var area_blocks = Variable.reg spec ~width:accumulator_bits in
    let%hw_var area_cells = Variable.reg spec ~width:accumulator_bits in
    let%hw_var tile_blocks = Variable.reg spec ~width:accumulator_bits in
    let%hw_var tile_cells = Variable.reg spec ~width:accumulator_bits in

    let reset =
      proc
        [
          area_blocks <--. 0;
          area_cells <--. 0;
          tile_blocks <--. 0;
          tile_cells <--. 0;
        ]
    in

    let%hw_var min_solution = Variable.reg spec ~width:output_bits in
    let%hw_var max_solution = Variable.reg spec ~width:output_bits in

    let tile_size = mux commands.tile_idx tile_sizes.tile_sizes in
    let new_tile_blocks =
      tile_blocks.value +: uresize commands.tile_count accumulator_bits
    in
    let new_tile_cells =
      tile_cells.value
      +: uresize (tile_size *: commands.tile_count) accumulator_bits
    in

    compile
      [
        when_ commands.new_grid
          [
            area_blocks
            <-- uresize
                  (div_3 commands.grid_rows *: div_3 commands.grid_cols)
                  accumulator_bits;
            area_cells
            <-- uresize
                  (commands.grid_rows *: commands.grid_cols)
                  accumulator_bits;
          ];
        when_
          (commands.add_tile &: ( ~: ) commands.finish_problem)
          [ tile_blocks <-- new_tile_blocks; tile_cells <-- new_tile_cells ];
        when_ commands.finish_problem
          [
            if_
              (area_blocks.value >=: new_tile_blocks)
              [
                min_solution <-- min_solution.value +:. 1;
                max_solution <-- max_solution.value +:. 1;
              ]
              [
                when_
                  (area_cells.value >=: new_tile_cells)
                  [ max_solution <-- max_solution.value +:. 1 ];
              ];
            reset;
          ];
      ];

    { min_solution = min_solution.value; max_solution = max_solution.value }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"solver" create
end

let create (scope : Scope.t) ({ clock; clear; data_in; data_in_valid } : _ I.t)
    : _ O.t =
  let tile_sizes =
    TileParser.hierarchical scope { clock; clear; data_in; data_in_valid }
  in
  let commands =
    ProblemParser.hierarchical scope { clock; clear; data_in; data_in_valid }
  in

  Solver.hierarchical scope { clock; clear; commands; tile_sizes }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day12" create
