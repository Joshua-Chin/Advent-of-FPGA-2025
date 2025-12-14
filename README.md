Day 1
=====

Part 1 & Part 2: Straightforward simulation of turning the dial.
We have one important optimization: instead of parsing the rotation as single integer, we parse the rotation as `(n / 100, n % 100)`.
Because the input is in decimal format, we can parse it as such without any explicit division or modulo operations.

Day 3
=====

Part 1 & Part 2: Keep an array of the largest `n` digit joltage value while processing the input as a stream.