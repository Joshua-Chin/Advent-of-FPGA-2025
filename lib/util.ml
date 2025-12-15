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
