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

let parse_int_list s =
  String.sub s ~pos:1 ~len:(String.length s - 2)
  |> String.split ~on:','
  |> List.map ~f:(fun i -> Int.of_string i)

type problem = { p1 : bool array; p2 : int array; buttons : int array array }

let parse_input input =
  input |> String.strip |> String.split_lines
  |> List.map ~f:(fun line ->
      let items = String.split line ~on:' ' |> List.to_array in
      (* Extract components *)
      let p1 = items.(0) in
      let buttons = Array.sub items ~pos:1 ~len:(Array.length items - 2) in
      let p2 = Array.last items in
      (* Parse components *)
      let p1 =
        String.sub p1 ~pos:1 ~len:(String.length p1 - 2)
        |> String.to_array
        |> Array.map ~f:(fun c -> Char.equal c '#')
      in
      let buttons = Array.map ~f:parse_int_list buttons in
      let p2 = parse_int_list p2 in
      {
        p1;
        p2 = List.to_array p2;
        buttons = Array.map ~f:List.to_array buttons;
      })

let rec gcd x y = if y = 0 then x else gcd y (x % y)
let lcm x y = match (x, y) with 0, _ | _, 0 -> 0 | _ -> abs (x * y) / gcd x y

let gaussian_elimination (m : int array array) : bool array =
  let rows = Array.length m in
  let cols = Array.length m.(0) in
  let is_free = Array.init (cols - 1) ~f:(fun _ -> true) in

  let start_row = ref 0 in
  for col = 0 to cols - 2 do
    (* Find a pivot. A pivot is row with the smallest non zero value *)
    let pivot = ref None in
    let pivot_val = ref Int.max_value in
    for row = !start_row to rows - 1 do
      let size = m.(row).(col) in
      if size <> 0 && size < !pivot_val then (
        pivot := Some row;
        pivot_val := size)
    done;

    Option.iter !pivot ~f:(fun pivot ->
        (* This pivot column is bound *)
        is_free.(col) <- false;
        (* Zero out the other rows *)
        let pivot_row = m.(pivot) in

        for row = 0 to rows - 1 do
          let row_val = m.(row).(col) in
          if row <> pivot && row_val <> 0 then
            let target = lcm (abs !pivot_val) (abs row_val) in
            let pivot_mul = target / !pivot_val in
            let row_mul = target / row_val in
            for i = 0 to cols - 1 do
              m.(row).(i) <-
                (row_mul * m.(row).(i)) - (pivot_mul * pivot_row.(i))
            done
        done;
        (* Swap the pivot into position *)
        m.(pivot) <- m.(!start_row);
        m.(!start_row) <- pivot_row;
        start_row := !start_row + 1)
  done;

  (* Simplify rows, if possible *)
  for row = 0 to rows - 1 do
    let is_positive = ref false in
    let div = ref 0 in
    for col = 0 to cols - 1 do
      let x = m.(row).(col) in
      if x <> 0 then
        if !div = 0 then (
          div := abs x;
          is_positive := x > 0)
        else div := gcd !div (abs x)
    done;
    if !div <> 0 then (
      if not !is_positive then div := - !div;
      for col = 0 to cols - 1 do
        m.(row).(col) <- m.(row).(col) / !div
      done)
  done;
  is_free

let compute_bounds buttons target =
  Array.map buttons ~f:(fun button ->
      Array.map button ~f:(fun idx -> target.(idx)) |> Array.reduce_exn ~f:min)

let tighten_bounds (m : int array array) (is_free : bool array)
    (initial_upper_bounds : int array) : int array * int array =
  let cols = Array.length m.(0) in
  let free_idxs =
    Array.filter_mapi is_free ~f:(fun i b -> if b then Some i else None)
  in
  let dependent_idxs =
    Array.filter_mapi is_free ~f:(fun i b -> if not b then Some i else None)
  in
  let lower = Array.map free_idxs ~f:(fun _ -> 0) in
  let upper = Array.map free_idxs ~f:(fun i -> initial_upper_bounds.(i)) in
  let tightened = ref true in
  while !tightened do
    tightened := false;
    (* Update bounds for each free variable *)
    Array.iteri free_idxs ~f:(fun bounds_idx free_idx ->
        (* Consider each row *)
        Array.iteri dependent_idxs ~f:(fun row _ ->
            let free_val = m.(row).(free_idx) in
            if free_val <> 0 then
              (* Determine if we are computing an upper or lower bounds*)
              let is_upper_bounds = free_val > 0 in
              (* Comopute the rhs *)
              let free_rhs =
                Array.mapi free_idxs ~f:(fun bounds_idx col ->
                    if col = free_idx then 0
                    else
                      let coef = m.(row).(col) in
                      (* If lower and coef is pos then we want lower *)
                      if coef > 0 then -coef * lower.(bounds_idx)
                      else -coef * upper.(bounds_idx))
                |> Array.reduce_exn ~f:( + )
              in
              let fixed_rhs = m.(row).(cols - 1) in
              let rhs = free_rhs + fixed_rhs in
              let bounds =
                Int.round_down rhs ~to_multiple_of:(abs free_val) / free_val
              in
              if is_upper_bounds then (
                if bounds < upper.(bounds_idx) then (
                  upper.(bounds_idx) <- bounds;
                  tightened := true))
              else if bounds > lower.(bounds_idx) then (
                lower.(bounds_idx) <- bounds;
                tightened := true)))
  done;
  (lower, upper)

let () =
  let search_spaces = Hashtbl.create (module Int) in
  load_input () |> parse_input
  |> List.iter ~f:(fun problem ->
      (* Construct the augmented matrix *)
      let dims = Array.length problem.p1 in
      let matrix =
        Array.init dims ~f:(fun _ ->
            Array.init (Array.length problem.buttons + 1) ~f:(fun _ -> 0))
      in
      (* Set the buttons *)
      Array.iteri problem.buttons ~f:(fun idx button ->
          Array.iter button ~f:(fun dim -> matrix.(dim).(idx) <- 1));
      (* Set the columns *)
      Array.iteri problem.p2 ~f:(fun dim count ->
          matrix.(dim).(Array.length problem.buttons) <- count);

      (* Perform Gaussian Elimination *)
      let is_free = gaussian_elimination matrix in

      (* Compute the bounds *)
      let bounds = compute_bounds problem.buttons problem.p2 in
      let search_space =
        Array.map2_exn is_free bounds ~f:(fun i b -> if i then b + 1 else 1)
        |> Array.reduce_exn ~f:( * )
      in

      (* Tighten the bounds *)
      let lower, upper = tighten_bounds matrix is_free bounds in
      let tight_search_space =
        Array.map2_exn lower upper ~f:(fun l r -> r - l + 1)
        |> Array.reduce ~f:( * ) |> Option.value ~default:1
      in
      Hashtbl.incr search_spaces tight_search_space;

      (* Tighten the bounds *)
      print_endline "New Problem";
      printf "Search Space: %d\n" search_space;
      printf "Tightened Search Space: %d\n" tight_search_space;
      print_string "Bounds:\n";
      print_s (Array.sexp_of_t Int.sexp_of_t lower);
      print_s (Array.sexp_of_t Int.sexp_of_t upper);
      printf "Free Variables: %d\n" (Array.count is_free ~f:(fun x -> x));
      print_s (Array.sexp_of_t (Array.sexp_of_t Int.sexp_of_t) matrix);
      print_endline "")
  |> ignore;
  print_s (Hashtbl.sexp_of_t Int.sexp_of_t Int.sexp_of_t search_spaces);
  printf "%d\n"
    (Hashtbl.to_alist search_spaces
    |> List.map ~f:(fun (a, b) -> a * b)
    |> List.reduce_exn ~f:( + ))
