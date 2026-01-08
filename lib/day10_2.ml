open! Base
open! Hardcaml
open! Signal

let max_dim = 4
let max_buttons = 6
let output_bits = 16
let max_free_variables = 3
let max_target = 400
let target_bits = address_bits_for max_target
let max_test_cases = 200
let elem_bits = 13

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    data_in : 'a; [@bits 8]
    data_in_valid : 'a;
  }
  [@@deriving hardcaml]
end

module Parser = struct
  module I = I

  module TestCase = struct
    type 'a t = {
      num_buttons : 'a; [@bits address_bits_for max_buttons]
      buttons : 'a list; [@bits max_dim] [@length max_buttons]
      target : 'a list; [@bits target_bits] [@length max_dim]
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { test_case_valid : 'a; test_case : 'a TestCase.t }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Parse_buttons | Parse_target | Finalize
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t)
      ({ clock; clear; data_in; data_in_valid } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in

    (* accumulator *)
    let accum = TestCase.Of_always.reg spec in
    let decimal = Variable.reg spec ~width:target_bits in
    let dim_idx = Variable.reg spec ~width:(address_bits_for max_dim) in

    let set_button =
      let button = List.hd_exn accum.buttons in
      proc
        [
          button
          <-- (button.value |: uresize (binary_to_onehot decimal.value) max_dim);
          decimal <--. 0;
        ]
    in

    let set_dim =
      [
        List.mapi accum.target ~f:(fun i b ->
            when_ (dim_idx.value ==:. i) [ b <-- decimal.value ])
        |> proc;
        decimal <--. 0;
        dim_idx <-- dim_idx.value +:. 1;
      ]
      |> proc
    in

    let reset =
      [
        TestCase.Of_always.assign accum (TestCase.Of_signal.of_int 0);
        decimal <--. 0;
        dim_idx <--. 0;
      ]
      |> proc
    in

    compile
      [
        when_ data_in_valid
          [
            sm.switch
              [
                ( Parse_buttons,
                  [
                    when_
                      (data_in ==:. Char.to_int '{')
                      [ sm.set_next Parse_target ];
                    when_
                      (data_in ==:. Char.to_int '(')
                      [
                        Util.shift_push (zero max_dim) accum.buttons;
                        accum.num_buttons <-- accum.num_buttons.value +:. 1;
                      ];
                    when_
                      (data_in ==:. Char.to_int ','
                      |: (data_in ==:. Char.to_int ')'))
                      [ set_button ];
                    Util.try_parse_to_digit decimal data_in;
                  ] );
                ( Parse_target,
                  [
                    when_
                      (data_in ==:. Char.to_int '}')
                      [ set_dim; sm.set_next Finalize ];
                    when_ (data_in ==:. Char.to_int ',') [ set_dim ];
                    Util.try_parse_to_digit decimal data_in;
                  ] );
                (Finalize, [ reset; sm.set_next Parse_buttons ]);
              ];
          ];
      ];

    {
      O.test_case_valid = sm.is Finalize;
      test_case = TestCase.Of_always.value accum;
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"parser" create
end

module GF8191 = struct
  let modulo = 8191

  let add x y =
    let rs x = uresize x 14 in
    let raw_sum = rs x +: rs y in
    mux2 (raw_sum >=:. 8191) (lsbs raw_sum -:. 8191) (lsbs raw_sum)

  let neg x =
    let is_zero = x ==:. 0 in
    mux2 is_zero (zero elem_bits) (of_int modulo ~width:elem_bits -: x)

  let sub x y = add x (neg y)

  let mul x y =
    let raw_mul = x *: y in
    let upper, lower = split_in_half_msb raw_mul in
    add upper lower

  (* TODO: Ensure this compiles to ROM *)
  let precomputed_mul_inverse =
    List.init modulo ~f:(fun i ->
        Z.powm (Z.of_int i) (Z.of_int @@ (modulo - 2)) (Z.of_int modulo)
        |> Z.to_int |> of_int ~width:elem_bits)

  let mul_inverse x = mux x precomputed_mul_inverse
  let mul_pow2 x n = rotl x n
end

let list_assign registers list =
  let open Always in
  List.map2_exn registers list ~f:( <-- ) |> proc

module GaussianElimination = struct
  module I = struct
    type 'a t = { clock : 'a; clear : 'a; commands : 'a Parser.O.t }
    [@@deriving hardcaml]
  end

  module NullSpace = struct
    type 'a t = {
      free_variables : 'a list;
          [@bits elem_bits] [@length max_free_variables * max_dim]
      constants : 'a list; [@bits elem_bits] [@length max_dim]
      upper_bounds : 'a list; [@bits target_bits] [@length max_free_variables]
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { null_space_valid : 'a; null_space : 'a NullSpace.t }
    [@@deriving hardcaml]
  end

  let test_case_to_matrix (test_case : _ Parser.TestCase.t) =
    let columns = test_case.buttons |> List.map ~f:(split_lsb ~part_width:1) in
    let rows = List.transpose_exn columns in
    List.map2_exn rows test_case.target ~f:(fun row target ->
        List.mapi (row @ [ gnd ]) ~f:(fun col cell ->
            mux2
              (test_case.num_buttons ==:. col)
              (uresize target elem_bits) (uresize cell elem_bits)))

  let compute_upper_bounds (test_case : _ Parser.TestCase.t) =
    let min a b = mux2 (a <: b) a b in
    List.map test_case.buttons ~f:(fun button ->
        let button = split_lsb button ~part_width:1 in
        List.map2_exn button test_case.target ~f:(fun valid value ->
            mux2 valid value (ones target_bits))
        |> tree ~arity:2 ~f:(reduce ~f:min))

  let find_pivot rows ignore_mask =
    let head = List.map rows ~f:List.hd_exn in
    let head_non_zero = List.map head ~f:(fun x -> x <>:. 0) in
    let priority_bits =
      List.map2_exn head_non_zero ignore_mask ~f:(fun non_zero ignore ->
          non_zero &: ( ~: ) ignore)
    in
    priority_select
    @@ List.mapi priority_bits ~f:(fun idx valid ->
        let value = of_int idx ~width:(address_bits_for max_dim) in
        { With_valid.valid; value })

  let assign_to_matrix registers matrix =
    let open Always in
    List.map2_exn registers matrix ~f:(fun reg_row mat_row ->
        List.map2_exn reg_row mat_row ~f:(fun reg_cell mat_cell ->
            reg_cell <-- mat_cell)
        |> proc)
    |> proc

  module States = struct
    type t =
      | Idle
      | Select_pivot
      | Normalize_pivot
      | Eliminate
      | Shift
      | Pop_shift
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) ({ clock; clear; commands } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in

    (* Input Fifo *)
    let%hw_var read_enable = Variable.wire ~default:gnd in
    let fifo =
      Fifo.create ~showahead:true ~clock ~clear ~wr:commands.test_case_valid
        ~d:(Parser.TestCase.Of_signal.pack commands.test_case)
        ~rd:read_enable.value ~capacity:max_test_cases ()
    in

    (* State variables *)
    let%hw_var num_columns =
      Variable.reg spec ~width:(address_bits_for max_buttons)
    in
    let done_mask =
      List.init max_dim ~f:(fun _ -> Variable.reg spec ~width:1)
    in
    let done_val = List.map ~f:Variable.value done_mask in

    let rows =
      List.init max_dim ~f:(fun _ ->
          List.init (max_buttons + 1) ~f:(fun _ ->
              Variable.reg spec ~width:elem_bits))
    in
    let rows_val = List.map rows ~f:(List.map ~f:Variable.value) in
    let curr_column =
      List.map rows ~f:(fun row -> List.hd_exn row |> Variable.value)
    in

    let upper_bounds =
      List.init max_buttons ~f:(fun _ -> Variable.reg spec ~width:target_bits)
    in

    (* Temporary pivot holder *)
    let pivot_idx_reg = Variable.reg spec ~width:(address_bits_for max_dim) in
    let pivot_row =
      List.init (max_buttons + 1) ~f:(fun _ ->
          Variable.reg spec ~width:elem_bits)
    in

    (* Output *)
    let accum = NullSpace.Of_always.reg spec in
    let output_valid = Variable.reg spec ~width:1 in

    (* Multiplicative inverse ROM *)
    let mul_inverse_address = Variable.reg spec ~width:elem_bits in
    let mul_inverse_data = GF8191.mul_inverse mul_inverse_address.value in

    let pop_column =
      proc
        [
          List.map rows ~f:(Fn.flip Util.shift_pop (zero elem_bits)) |> proc;
          Util.shift_pop upper_bounds (zero target_bits);
          num_columns <-- num_columns.value -:. 1;
        ]
    in

    let reset =
      proc
        [
          num_columns <--. 0;
          List.map done_mask ~f:(fun r -> r <-- gnd) |> proc;
          output_valid <-- gnd;
          NullSpace.Of_always.assign accum (NullSpace.Of_signal.of_int 0);
        ]
    in

    compile
      [
        sm.switch
          [
            ( Idle,
              [
                reset;
                when_ (( ~: ) fifo.empty)
                  [
                    (* Read value from the FIFO *)
                    read_enable <-- vdd;
                    (let test_case = Parser.TestCase.Of_signal.unpack fifo.q in
                     proc
                       [
                         test_case_to_matrix test_case |> assign_to_matrix rows;
                         compute_upper_bounds test_case
                         |> list_assign upper_bounds;
                         num_columns <-- test_case.num_buttons;
                       ]);
                    sm.set_next Select_pivot;
                  ];
              ] );
            ( Select_pivot,
              [
                if_ (num_columns.value ==:. 0)
                  [
                    output_valid <-- vdd;
                    (* Update the constants *)
                    list_assign accum.constants curr_column;
                    sm.set_next Idle;
                  ]
                  [
                    (* Get pivot index *)
                    (let pivot_idx = find_pivot rows_val done_val in
                     if_ pivot_idx.valid
                       [
                         (* Update done mask *)
                         List.mapi done_mask ~f:(fun i r ->
                             when_ (pivot_idx.value ==:. i) [ r <-- vdd ])
                         |> proc;
                         pivot_idx_reg <-- pivot_idx.value;
                         (* Extract the pivot row *)
                         (let source_row =
                            List.map
                              (List.transpose_exn rows_val)
                              ~f:(mux pivot_idx.value)
                          in
                          proc
                            [
                              (* Copy the row to the pivot row registers *)
                              list_assign pivot_row source_row;
                              (* Retrieve the multiplicative inverse from ROM *)
                              mul_inverse_address <-- List.hd_exn source_row;
                            ]);
                         sm.set_next Normalize_pivot;
                       ]
                       [ sm.set_next Pop_shift ]);
                  ];
              ] );
            ( Normalize_pivot,
              [
                (* Normalize the pivot to start with 1 *)
                List.map pivot_row ~f:(fun elem ->
                    elem <-- GF8191.mul elem.value mul_inverse_data)
                |> proc;
                sm.set_next Eliminate;
              ] );
            ( Eliminate,
              [
                (* Eliminate the pivot column from the other rows *)
                List.mapi rows ~f:(fun i row ->
                    let head = List.hd_exn row |> Variable.value in
                    if_
                      (pivot_idx_reg.value ==:. i)
                      [
                        List.map2_exn row pivot_row ~f:(fun r x ->
                            r <-- x.value)
                        |> proc;
                      ]
                      [
                        List.map2_exn row pivot_row ~f:(fun r x ->
                            r <-- GF8191.sub r.value (GF8191.mul x.value head))
                        |> proc;
                      ])
                |> proc;
                sm.set_next Shift;
              ] );
            (Shift, [ pop_column; sm.set_next Select_pivot ]);
            ( Pop_shift,
              [
                (* Pop from the head of the matrix *)
                pop_column;
                (* Push to the free variables *)
                (let frees =
                   List.chunks_of ~length:max_dim accum.free_variables
                   |> List.transpose_exn
                 in
                 List.map2_exn curr_column frees ~f:Util.shift_push |> proc);
                sm.set_next Select_pivot;
                (* Push the upper bounds *)
                Util.shift_push
                  (List.hd_exn upper_bounds |> Variable.value)
                  accum.upper_bounds;
              ] );
          ];
      ];
    {
      O.null_space_valid = output_valid.value;
      null_space = NullSpace.Of_always.value accum;
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"ge" create
end

module Solver = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      commands : 'a GaussianElimination.O.t;
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      solution_valid : 'a;
      solution : 'a; [@bits elem_bits]
      config : 'a list; [@bits elem_bits] [@length max_dim]
    }
    [@@deriving hardcaml]
  end

  module NS = GaussianElimination.NullSpace

  let vecs_sub xs ys = List.map2_exn xs ys ~f:GF8191.sub

  let get_initial_config (null_space : _ NS.t) offset =
    if offset = 0 then null_space.constants
    else
      let free_variables =
        List.chunks_of ~length:max_dim null_space.free_variables
      in
      vecs_sub null_space.constants
        (List.map
           (List.hd_exn free_variables)
           ~f:(GF8191.mul (of_int offset ~width:elem_bits)))

  let get_next_idxs ~idxs ~bounds ~strides =
    List.fold_right2_exn idxs (List.zip_exn bounds strides)
      ~init:(vdd, [], [], [])
      ~f:(fun i (b, s) (carry, incr_mask, carry_mask, new_idxs) ->
        let saturated = i >=: b in
        let reset = carry &: saturated in
        let increment = carry &: ( ~: ) saturated in
        let new_idx =
          mux2 reset (zero (width i)) @@ mux2 increment (i +:. s) @@ i
        in
        (reset, increment :: incr_mask, carry :: carry_mask, new_idx :: new_idxs))

  let get_next_config ~cache ~coefs ~strides_log2 ~incr_mask =
    List.map2_exn cache (List.zip_exn coefs strides_log2) ~f:(fun c (d, s) ->
        vecs_sub c (List.map d ~f:(fun x -> GF8191.mul_pow2 x s)))
    |> List.transpose_exn
    |> List.map ~f:(fun xs ->
        priority_select
        @@ List.map2_exn incr_mask xs ~f:(fun valid value ->
            { With_valid.valid; value }))
    |> List.map ~f:(fun x -> x.value)

  let config_valid (config : _ list) =
    List.map config ~f:(fun x -> x <:. 512)
    |> tree ~arity:2 ~f:(reduce ~f:( &: ))

  module States = struct
    type t = Idle | Search [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create ?(stride_log2 = 1) ?(offset = 2) (scope : Scope.t)
      ({ clock; clear; commands } : _ I.t) : _ O.t =
    ignore scope;
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in

    (* State variables *)
    let free_variables =
      List.init max_free_variables ~f:(fun _ ->
          List.init max_dim ~f:(fun _ -> Variable.reg spec ~width:elem_bits))
    in
    let upper_bounds =
      List.init max_free_variables ~f:(fun _ ->
          Variable.reg spec ~width:target_bits)
    in
    let config =
      List.init max_dim ~f:(fun _ -> Variable.reg spec ~width:elem_bits)
    in
    let config_cache =
      List.init max_free_variables ~f:(fun _ ->
          List.init max_dim ~f:(fun _ -> Variable.reg spec ~width:elem_bits))
    in

    let idxs =
      List.init max_free_variables ~f:(fun _ ->
          Variable.reg spec ~width:target_bits)
    in

    (* Output *)
    let output_valid = Variable.reg spec ~width:1 in
    let output =
      Variable.reg
        (Reg_spec.override spec ~clear_to:(ones elem_bits))
        ~width:elem_bits
    in
    (* Logic Helper *)

    let list_values = List.map ~f:Variable.value in
    let list_reset rs = List.map ~f:(fun r -> r <--. 0) rs |> proc in

    let strides_log2 =
      stride_log2 :: List.init (max_free_variables - 1) ~f:(fun _ -> 0)
    in
    let strides = List.map ~f:(fun x -> 2 ** x) strides_log2 in

    let is_done, incr_mask, carry_mask, next_idxs =
      get_next_idxs ~idxs:(list_values idxs) ~bounds:(list_values upper_bounds)
        ~strides
    in

    let next_config =
      get_next_config
        ~cache:(List.map ~f:list_values config_cache)
        ~coefs:(List.map ~f:list_values free_variables)
        ~strides_log2 ~incr_mask
    in

    let next_config_cache =
      List.map2_exn carry_mask config_cache ~f:(fun r c ->
          List.map2_exn next_config (list_values c) ~f:(mux2 r))
    in

    let curr_sum =
      let idx_vals =
        list_values idxs |> List.map ~f:(fun x -> uresize x elem_bits)
      in
      let config_vals = list_values config in
      idx_vals @ config_vals |> tree ~arity:2 ~f:(reduce ~f:( +: ))
    in

    let reset =
      proc
        [
          list_reset @@ List.concat free_variables;
          list_reset upper_bounds;
          list_reset config;
          list_reset idxs;
          output <-- ones elem_bits;
          output_valid <-- gnd;
        ]
    in

    compile
      [
        sm.switch
          [
            ( Idle,
              [
                reset;
                when_ commands.null_space_valid
                  [
                    (let null_space = commands.null_space in
                     let initial_config = get_initial_config null_space offset in
                     proc
                       [
                         list_assign
                           (List.concat free_variables)
                           null_space.free_variables;
                         list_assign config initial_config;
                         List.map config_cache ~f:(fun row ->
                             list_assign row initial_config)
                         |> proc;
                         list_assign upper_bounds null_space.upper_bounds;
                       ]);
                    sm.set_next Search;
                  ];
              ] );
            ( Search,
              [
                when_
                  (config_valid (list_values config))
                  [ when_ (curr_sum <: output.value) [ output <-- curr_sum ] ];
                when_ is_done [ output_valid <-- vdd; sm.set_next Idle ];
                list_assign idxs next_idxs;
                list_assign config next_config;
                List.map2_exn config_cache next_config_cache ~f:list_assign
                |> proc;
              ] );
          ];
      ];
    {
      solution_valid = output_valid.value;
      solution = output.value;
      config = list_values config;
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"solver" create
end

module SolverCoordinator = struct
  module I = Solver.I
  module O = Solver.O

  module States = struct
    type t = Idle | Busy [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) ({ clock; clear; commands } : _ I.t) : _ O.t =
    let open Always in
    let spec = Reg_spec.create ~clock ~clear () in

    let sm = State_machine.create (module States) spec in
    (* Input FIFO *)
    let read_enable = Variable.wire ~default:gnd in
    let module NS = GaussianElimination.NullSpace in
    let fifo =
      Fifo.create ~showahead:true ~clock ~clear ~wr:commands.null_space_valid
        ~d:(NS.Of_signal.pack commands.null_space)
        ~rd:read_enable.value ~capacity:max_test_cases ()
    in
    let output =
      Solver.hierarchical scope
        {
          clock;
          clear;
          commands =
            {
              null_space = NS.Of_signal.unpack fifo.q;
              null_space_valid = read_enable.value;
            };
        }
    in
    compile
      [
        sm.switch
          [
            ( Idle,
              [
                when_ (( ~: ) fifo.empty)
                  [ read_enable <-- vdd; sm.set_next Busy ];
              ] );
            (Busy, [ when_ output.solution_valid [ sm.set_next Idle ] ]);
          ];
      ];
    output

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"coordinator" create
end

module O = struct
  type 'a t = { part2 : 'a; [@bits output_bits] part2_valid : 'a }
  [@@deriving hardcaml]
end

module Test = struct
  module I = GaussianElimination.I
  module O = Solver.O

  let create (scope : Scope.t) ({ clock; clear; _ } as input : _ I.t) : _ O.t =
    Solver.hierarchical scope
      { clock; clear; commands = GaussianElimination.hierarchical scope input }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"soltestver" create
end

let create (scope : Scope.t) ({ clock; clear; _ } as input : _ I.t) : _ O.t =
  let parser = Parser.hierarchical scope input in
  let gaussian_elim =
    GaussianElimination.hierarchical scope { clock; clear; commands = parser }
  in
  let solver =
    SolverCoordinator.hierarchical scope
      { clock; clear; commands = gaussian_elim }
  in
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in
  let output = Variable.reg spec ~width:output_bits in
  let progress_bits = address_bits_for max_test_cases in
  let in_progress = Variable.reg spec ~width:progress_bits in
  compile
    [
      when_ solver.solution_valid
        [ output <-- output.value +: uresize solver.solution output_bits ];
      in_progress
      <-- in_progress.value
          +: uresize parser.test_case_valid progress_bits
          -: uresize solver.solution_valid progress_bits;
    ];
  { part2 = output.value; part2_valid = in_progress.value ==:. 0 }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day10_2" create
