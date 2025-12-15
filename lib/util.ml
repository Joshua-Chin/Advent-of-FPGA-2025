open! Base
open! Hardcaml
open! Signal

let try_parse_to_digit current new_digit =
  Always.(
    when_
      (new_digit >=:. Char.to_int '0' &: (new_digit <=:. Char.to_int '9'))
      [
        current
        <-- uresize (current.value *: of_int 10 ~width:4) (width current.value)
            +: uresize (new_digit -:. Char.to_int '0') (width current.value);
      ])

let rec pairwise = function
  | x :: (y :: _ as rest) -> (x, y) :: pairwise rest
  | _ -> []

let compute_multiply_shift divisor upper_bounds =
  let open Z in
  let divisor = of_int divisor in
  let upper_bounds = of_int upper_bounds in
  let x = ((upper_bounds + one) / divisor * divisor) - one in

  let test p =
    let two_p = shift_left one p in
    let k = rem (divisor - rem two_p divisor) divisor in
    if gt two_p (x * k) then Some (to_int ((two_p + k) / divisor), p) else None
  in
  let start = numbits upper_bounds in
  let stop = Int.( + ) start (Int.( + ) (numbits divisor) 3) in
  Sequence.range start stop |> Sequence.find_map ~f:test |> Option.value_exn

let bit_length x = Int.floor_log2 x + 1
