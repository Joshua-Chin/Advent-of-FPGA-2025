open! Base
open! Hardcaml
open! Signal

let max_dim = 10
let max_buttons = 13
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

module TestCase = struct
  type 'a t = {
    num_buttons : 'a; [@bits address_bits_for max_buttons]
    buttons : 'a list; [@bits max_dim] [@length max_buttons]
    target : 'a list; [@bits target_bits] [@length max_dim]
  }
  [@@deriving hardcaml]
end

module Parser = struct
  module I = I

  module O = struct
    type 'a t = { valid : 'a; test_case : 'a TestCase.t } [@@deriving hardcaml]
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

    { O.valid = sm.is Finalize; test_case = TestCase.Of_always.value accum }

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"parser" create
end

let add_mod x y =
  let rs x = uresize x 14 in
  let raw_sum = rs x +: rs y in
  mux2 (msb raw_sum) (lsbs raw_sum +:. 1) (lsbs raw_sum)

let neg_mod x =
  let is_zero = x ==:. 0 in
  mux2 is_zero (zero elem_bits) (of_int modulo ~width:elem_bits -: x)

let sub_mod x y = add_mod x (neg_mod y)

let mul_mod x y =
  let raw_mul = x *: y in
  let upper, lower = split_in_half_msb raw_mul in
  add_mod upper lower

module NullSpace = struct
  type 'a t = {
    f1 : 'a list; [@bits elem_bits] [@length max_dim]
    f2 : 'a list; [@bits elem_bits] [@length max_dim]
    f3 : 'a list; [@bits elem_bits] [@length max_dim]
    constants : 'a list; [@bits elem_bits] [@length max_dim]
    upper_bounds : 'a list; [@bits elem_bits] [@length 3]
  }
  [@@deriving hardcaml]
end

module GaussianElimination = struct
  module I = struct
    type 'a t = { clock : 'a; clear : 'a; commands : 'a Parser.O.t }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { valid : 'a; null_space : 'a NullSpace.t }
    [@@deriving hardcaml]
  end

  (* Inputs must be registered *)
  let mul_inverse x =
    mux x
    @@ List.init (modulo - 1) ~f:(fun i ->
        Z.powm (Z.of_int i) (Z.of_int @@ (modulo - 2)) (Z.of_int modulo)
        |> Z.to_int |> of_int ~width:elem_bits)

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

    let%hw_var read_enable = Variable.wire ~default:gnd in
    let fifo =
      Fifo.create ~showahead:true ~clock ~clear ~wr:commands.valid
        ~d:(TestCase.Of_signal.pack commands.test_case)
        ~rd:read_enable.value ~capacity:max_test_cases ()
    in

    let%hw_var num_columns =
      Variable.reg spec ~width:(address_bits_for max_buttons)
    in
    let matrix =
      List.init max_dim ~f:(fun _ ->
          List.init max_buttons ~f:(fun _ -> Variable.reg spec ~width:elem_bits))
    in

    let target =
      List.init max_dim ~f:(fun _ -> Variable.reg spec ~width:elem_bits)
    in

    let pivot_index = Variable.reg spec ~width:(address_bits_for max_dim) in

    let pivot_row =
      List.init max_buttons ~f:(fun _ -> Variable.reg spec ~width:elem_bits)
    in
    let pivot_target = Variable.reg spec ~width:elem_bits in

    let get_column = List.map matrix ~f:(fun row -> List.hd_exn row) in

    let%hw_var done_mask = Variable.reg spec ~width:max_dim in

    let accum = NullSpace.Of_always.reg spec in
    let output_valid = Variable.reg spec ~width:1 in

    let mul_inverse_address = Variable.reg spec ~width:elem_bits in
    let mul_inverse_data = mul_inverse mul_inverse_address.value in

    let pop_column =
      proc
        [
          List.map matrix ~f:(Fn.flip Util.shift_pop (zero elem_bits)) |> proc;
          num_columns <-- num_columns.value -:. 1;
        ]
    in

    compile
      [
        sm.switch
          [
            ( Idle,
              [
                output_valid <-- gnd;
                NullSpace.Of_always.assign accum (NullSpace.Of_signal.of_int 0);
                when_ (( ~: ) fifo.empty)
                  [
                    read_enable <-- vdd;
                    (let test_case = TestCase.Of_signal.unpack fifo.q in
                     proc
                       [
                         num_columns <-- test_case.num_buttons;
                         List.map2_exn target test_case.target ~f:(fun r x ->
                             r <-- x)
                         |> proc;
                         List.map2_exn (List.transpose_exn matrix)
                           test_case.buttons ~f:(fun rs xs ->
                             List.map2_exn rs (split_lsb ~part_width:1 xs)
                               ~f:(fun r x -> r <-- uresize x elem_bits)
                             |> proc)
                         |> proc;
                       ]);
                    sm.set_next Select_pivot;
                  ];
              ] );
            ( Select_pivot,
              [
                if_ (num_columns.value ==:. 0)
                  [
                    output_valid <-- vdd;
                    done_mask <--. 0;
                    (* Update the constants *)
                    List.map2_exn accum.constants target ~f:(fun x y ->
                        x <-- Variable.value y)
                    |> proc;
                    sm.set_next Idle;
                  ]
                  [
                    (* Get pivot index *)
                    (let mask =
                       List.map get_column ~f:(fun x -> x.value <>:. 0)
                       |> List.map2_exn
                            (split_lsb ~part_width:1 done_mask.value)
                            ~f:(fun is_non_zero is_done ->
                              is_non_zero &: ( ~: ) is_done)
                     in
                     let pivot_idx =
                       priority_select
                       @@ List.mapi mask ~f:(fun idx valid ->
                           {
                             With_valid.valid;
                             value =
                               of_int idx ~width:(address_bits_for max_dim);
                           })
                     in
                     if_ pivot_idx.valid
                       [
                         (* Update done mask *)
                         done_mask
                         <-- (done_mask.value
                             |: (binary_to_onehot pivot_idx.value
                                |> Fn.flip uresize max_dim));
                         pivot_index <-- pivot_idx.value;
                         (* Extract pivot row + target *)
                         (let source_row =
                            List.map (List.transpose_exn matrix) ~f:(fun col ->
                                mux pivot_idx.value
                                  (List.map col ~f:Variable.value))
                          in
                          proc
                            [
                              List.map2_exn pivot_row source_row ~f:( <-- )
                              |> proc;
                              pivot_target
                              <-- mux pivot_idx.value
                                    (List.map target ~f:Variable.value);
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
                List.map (pivot_target :: pivot_row) ~f:(fun elem ->
                    elem <-- mul_mod elem.value mul_inverse_data)
                |> proc;
                sm.set_next Eliminate;
              ] );
            ( Eliminate,
              [
                List.mapi (List.zip_exn matrix target)
                  ~f:(fun i (partial_row, t) ->
                    let row = t :: partial_row in
                    let head = List.hd_exn partial_row |> Variable.value in
                    let pivot_row = pivot_target :: pivot_row in
                    if_ (pivot_index.value ==:. i)
                      [
                        List.map2_exn row pivot_row ~f:(fun r x ->
                            r <-- x.value)
                        |> proc;
                      ]
                      [
                        List.map2_exn row pivot_row ~f:(fun r x ->
                            r <-- sub_mod r.value (mul_mod x.value head))
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
                (let frees = [ accum.f1; accum.f2; accum.f3 ] in
                 List.map2_exn
                   (List.map get_column ~f:Variable.value)
                   (List.transpose_exn frees) ~f:Util.shift_push
                 |> proc);
                sm.set_next Select_pivot;
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

module O = struct
  type 'a t = { part2 : 'a [@bits output_bits] } [@@deriving hardcaml]
end

let create (scope : Scope.t) (_ : _ I.t) : _ O.t =
  ignore scope;
  { part2 = gnd }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day10_2" create
