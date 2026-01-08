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
let modulo = 8191
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
  let mul_inverse x =
    mux x
    @@ List.init (modulo - 1) ~f:(fun i ->
        Z.powm (Z.of_int i) (Z.of_int @@ (modulo - 2)) (Z.of_int modulo)
        |> Z.to_int |> of_int ~width:elem_bits)
end

module GaussianElimination = struct
  module I = struct
    type 'a t = { clock : 'a; clear : 'a; commands : 'a Parser.O.t }
    [@@deriving hardcaml]
  end

  module NullSpace = struct
    type 'a t = {
      f1 : 'a list; [@bits elem_bits] [@length max_dim]
      f2 : 'a list; [@bits elem_bits] [@length max_dim]
      f3 : 'a list; [@bits elem_bits] [@length max_dim]
      constants : 'a list; [@bits elem_bits] [@length max_dim]
      upper_bounds : 'a list; [@bits target_bits] [@length max_free_variables]
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { valid : 'a; null_space : 'a NullSpace.t }
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
                         |> List.map2_exn upper_bounds ~f:( <-- )
                         |> proc;
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
                    List.map2_exn accum.constants curr_column ~f:( <-- ) |> proc;
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
                              List.map2_exn pivot_row source_row ~f:( <-- )
                              |> proc;
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
                   [ accum.f1; accum.f2; accum.f3 ] |> List.transpose_exn
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
      O.valid = output_valid.value;
      null_space = NullSpace.Of_always.value accum;
    }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"ge" create
end

module O = GaussianElimination.O

let create (scope : Scope.t) ({ clock; clear; _ } as input : _ I.t) : _ O.t =
  let parser = Parser.hierarchical scope input in
  let gaussian_elim =
    GaussianElimination.hierarchical scope { clock; clear; commands = parser }
  in
  gaussian_elim

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day10_2" create
