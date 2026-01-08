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

let shift_push new_val shift_buffer =
  let open Always in
  let rec f new_val shift_buffer =
    match shift_buffer with
    | r :: rest -> (r <-- new_val) :: f r.value rest
    | _ -> []
  in
  proc @@ f new_val shift_buffer

let shift_pop shift_buffer default =
  let open Always in
  let rec f shift_buffer =
    match shift_buffer with
    | r :: rest ->
        let new_val, actions = f rest in
        (Variable.value r, (r <-- new_val) :: actions)
    | _ -> (default, [])
  in
  let _, actions = f shift_buffer in
  proc actions

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

module type SimpleDualPortRamConfig = sig
  val size : int
  val item_width : int
end

module SimpleDualPortRam (C : SimpleDualPortRamConfig) = struct
  let address_bits = address_bits_for C.size

  module I = struct
    type 'a t = {
      write_address : 'a; [@bits address_bits]
      write_enable : 'a;
      write_data : 'a; [@bits C.item_width]
      read_address : 'a; [@bits address_bits]
      read_enable : 'a;
    }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) ~clock
      ({ write_address; write_enable; write_data; read_address; read_enable } :
        _ I.t) =
    ignore scope;
    let write_port =
      {
        Write_port.write_clock = clock;
        write_address;
        write_enable;
        write_data;
      }
    in
    let read_port =
      { Read_port.read_clock = clock; read_address; read_enable }
    in
    let q =
      Ram.create ~collision_mode:Read_before_write ~size:C.size
        ~write_ports:[| write_port |] ~read_ports:[| read_port |] ()
    in
    q.(0)
end
