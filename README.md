Day 1
=====

Part 1 & Part 2: Fairly straightforward simulation of turning the dial.
We have one important optimization: instead of parsing the rotation as single integer, we parse the rotation as `(n / 100, n % 100)`.
Because the input is in decimal format, we can parse it as such without any explicit division or modulo operations.