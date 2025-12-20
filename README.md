# Advent of FPGA 2025

This repository contains my solutions to Advent of Code 2025 implemented in Hardcaml.

## Approach

Each solution parses the input file one character at a time.
The solutions were validated against my official puzzle inputs using Cyclesim.
Both parts of the problem are solved with the same circuit.

To run a solution, you can use the following command.
```
dune exec bin/dayXX.exe path/to/input/file.txt
```

## Directory Structure
```
.
├── bin/
│   └── dayXX.ml    # Binaries for running a particular day's solution
└── lib/
    ├── dayXX.ml    # Module defining the solution for a particular day
    ├── runner.ml   # Functor for generating runners.
    └── util.ml     # Utility functions used by multiple days.
```

## Highlighted Solutions

### Day 10, Part 1
The problem asks for the minimal Hamming weight solution to a system of linear equations over GF(2).

Because the inputs are roughly square (a similar number of buttons and dimensions), I apply Gaussian elimination, then brute force over the free variables.

When loading the problem, I store the augmented matrix in an array of shift buffers, with each buffer representing a row.

During Gaussian elimination, I process each column a single cycle.
Within that cycle, I pop the first column of the matrix, shifting each buffer left.
Columns corresponding to free variables are forwarded to the next stage.
This shifting process significantly reduces the amount of wiring.
I also maintain a `done` mask over the rows to avoid swapping rows.

The brute force stage receives the columns corresponding to the free variables, and the RHS constants.
For a given configuration of free variables, computing the dependent variables is a simple matrix-vector product.
Because the number of free variables is small (less than 5 over my puzzle input) , I instantiate an instance for each possible configuration of free variables.

My design avoids buffering / backpressure logic by exploiting the input format.
The Gaussian elimination circuit requires `dimensions` cycles to compute.
However, every input line concludes with data for Part 2 (e.g. `{3,5,4,7}`), which takes at least `2 * dimensions` cycles to parse.
By running the Gaussian elimination circuit concurrently with the Part 2 parsing, its latency is fully masked, guaranteeing it is available before the next problem starts.

### Day 2
This problem asks us to compute the number of n-repeats in a list of ranges.

I approached this by first implementing a circuit for a fixed number of digits and repeats.
That circuit only requires a fixed number of divisions by a constant and multiplications.
Because the range for each solver is bounded, I implemented my own function to precompute the multiply-shift values to take advantage of those bounds.

For part 1, which asks for only 2-repeats, I simply instantiated separate instances for each digit count up to the maximum, and computed the sum.

For part 2, we can instantiate separate instances for each digit count and divisor of that digit count.
However, we need to avoid overcounting by performing inclusion-exclusion on the divisors.
Conveniently, we can apply a Mobius transform to simplify the pre-computation of the coefficients.
My solution allows the user to set an arbitrary maximum digit count during generation, and will compute the coefficients on the fly.

The output is a continuous stream, with a 7 cycle delay.

### Day 8, Part 2
While the problem description implies an edge-centric approach (Kruskal's algorithm), sorting O(N^2) edges is inefficient in hardware.
Finding the last edge connected in Kruskal's algorithm is equivalent to finding the longest edge in the minimum spanning tree.
Therefore, to improve the parallelism, I used Primm's algorithm, while tracking the largest edge added so far.

We fully parallelize the distance computations, determining a new edge in the MST in a fixed number of clock cycles.
This circuit runs in `O(n)` cycles, instead of the typical `O(n^2)` cycles that a Kruskal's algorithm approach might use.
This approach represents a significant area vs latency tradeoff, using a very large number of DSP slices to minimize total runtime.
If fewer DSP slices are available, we can instead process 

To reduce routing congestion, the points are stored both in registers and BRAM.
The instance in registers is used for the parallel distance calculation, while the instance in BRAM is used for retrieving the coordinates of the newly added point.

## Solution Descriptions

### Day 1
I avoid hardware division or modulo operations by parsing the ASCII decimal rotation as the triple `(sign, magnitude / 100, magnitude % 100)`.

The output is a continuous stream, with a 1 cycle delay.

### Day 3
I maintain an array of joltages where each index represents the maximum achievable joltage so far using exactly an "index" number of batteries.

The output is a continuous stream, with a 1 cycle delay.

### Day 4
I treat this as a cellular automata, with all cells updating in parallel in a single cycle.
I support arbitrary dimensions by using the first row as a shift buffer, and shifting down when a new row starts.

### Day 5
I store each range in its own pair of registers, pushing into a shift buffer.
For Part 1, I compare each ingredient against all pairs in parallel.

For Part 2, I sort the ranges by their start as they come in, using insertion sort and a systolic array.
Once all the ranges have been parsed, I iterate through the ranges, treating it as a circular shift buffer.
That allows the solution to solve Part 1 and Part 2 simultaneously, while sharing the same set of registers for the ranges.

For Part 1, the output is a continuous stream, with a 1 cycle delay.
For Part 2, the output is a batch process that finishes before the input is done streaming on most test cases.

### Day 6
In Part 1, I maintain two line buffers over the blocks - one for the current sum and one for the product.
In Part 2, I maintain a line buffer over each column, then perform the operations while reading the last row.

### Day 7
The number of paths through a cell can be computed using only information of the above 3 cells (left, center, right).
I maintain a line buffer in BRAM storing the number of paths and whether the cell has a splitter for each column.
I then run a sliding window as the input streams in.

### Day 12
At first, this problem appears to be a grid packing problem that will require an ILP / constraint satisfaction approach.
However, the test cases are trivial.
Either there are too many individual cells required to fit in the grid, or the grid can fit a number of 3x3 bounding boxes greater than or equal to the number of tiles.
The solution solves each test case in a constant number of cycles.