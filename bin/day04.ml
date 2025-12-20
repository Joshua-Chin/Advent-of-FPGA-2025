module Day04 = struct
  module I = Advent_of_fpga_2025.Day04.I
  module O = Advent_of_fpga_2025.Day04.O
  let hierarchical = Advent_of_fpga_2025.Day04.hierarchical ?config:None
end

module M = Advent_of_fpga_2025.Runner.Make(Day04)

let () = M.run ()