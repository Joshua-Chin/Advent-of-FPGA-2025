Day 1
=====

Part 1 & Part 2: Straightforward simulation of turning the dial.
We have one important optimization: instead of parsing the rotation as single integer, we parse the rotation as `(n / 100, n % 100)`.
Because the input is in decimal format, we can parse it as such without any explicit division or modulo operations.

Day 2
=====

A big step up in difficulty. We instantiate submodules that counts the sum of invalid ID's in the range for a fixed number of digits and repetitions. We implement a number of optimizations.

Each module needs to divide by a fixed number (e.g. 1001 or 10101). We determine a minimum multiplier and shift dictated by the maximum number of digits.

We determine which modules we need instantiate by computing the divisors of each digits and determining the mobius function of that divisor.

Day 3
=====

Part 1 & Part 2: Keep an array of the largest `n` digit joltage value while processing the input as a stream.

Day 4
=====

We read the data into registers, then process all the cell in parallel. The number of changes is calculated by a pipelined sum.

Day 5
=====
We read the ranges into registers. To solve part 1, we compare each ingredient against all ranges simultaneously. To solve part 2, we sort the ranges as they come with insertion sort / a systolic array. Then, while simultaneously processing the ingredients, we process the sorted ranges with a ciricular shift buffer.

Day 6
=====

Part 1: We maintain two line buffers - one for the current sum of each block and one for the product. In the final row, we mux between the two.
Part 2: We maintain a line buffer contiaining the current value for each column. In the final row, we perform the operation.
(One potential optimization is pipeline the multiplies in the final row, but I'm happy enough with the solution. We could either read into a buffer, then perform a multiply tree.)

Day 7
=====
We maintain a kernel of the above 3 values (left center right), keeping the previous line in a line buffer.

Day 8
=====
We apply Primms algorithm, computing the distances in parallel. The maximum length edge in the MST is the last edge connected in the simulation.

Day 10
======
We apply Gaussian elimination, the search the entire free variable space in a single cycle.

Day 12
======
This difficulty of this question is about parsing. While the logic is straightforward, properly modularizing makes it much cleaner.

