# Advent of FPGA 2025

This repository contains my solutions to Advent of Code 2025 implemented in Hardcaml.

## Approach

Each solution parses the input file one character at a time.
The solutions were validated against my official puzzle inputs using Cyclesim.
Both parts of the problem are solved with the same circuit.

To run a solution, first install the dependencies:
```
opam install . --deps-only
```

Then run the following command:
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

Because there are a similar number of buttons and dimensions (the augmented matrix is roughly square), I apply Gaussian elimination, then brute force over the free variables.

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
By running the Gaussian elimination circuit concurrently with input for part 2, its latency is fully masked, guaranteeing it is available before the next problem starts.

### Day 10, Part 2

The problem asks for the minimal Hamming weight solution to a system of linear equations over the integers.
Each variable is a non-negative integer with 0-1 coefficients.

From observation, I see that there are up to 13 variables, 10 equations, and the constant terms are in the range `[0, 400]`.
Note that because each variable is non-negative with 0-1 coefficients I can bound them from above by the constant terms.

I use a similar approach to part 1, performing Gaussian elimination then brute forcing the search space.
Instead of performing fraction-free Gauss-Jordan elimination over the integers, I perform Gaussian elimination over `GF(8191)`.
Computing the Gaussian elimination over a finite field makes the math much easier, avoiding GCD calculating and potentially very large intermediate values.
If all variables are in the range `[0, 400]`, then a solution over `GF(8191)` is also a solution over the integers.

> Proof: If $x$ is a solution over $GF(8191)$, then we have $Ax - b \equiv 0 \mod{8191}$.
This is equivalent to $Ax - b + 8191y = 0$ for some integer $y$.
Because $a,b$ are in the range $[0, 400]$ and there are up to $13$ variables, The LHS of the system $Ax-b$ is in the range $[-400, 5200]$.
Because $5200 < 8191$, $y = 0$, $Ax - b = 0$, and $x$ is a solution over the integers.

I selected `8191` as the modulus because it is the Mersenne prime $2^{13} - 1$. This makes applying the modulus on the result of multiplication and addition very straightforward.

In the Gaussian elimination circuit, I have a setup similar to part 1, where the matrix is stored in a set of shift buffers, with each representing a row. Division is handled by a lookup into a ROM containing the precomputed modular inverse.

In the exploration of the search space, I use a differential approach, updating the previous values of the dependent variables incrementally instead of recomputing them at each iteration.
This lets me avoid the use of any DSP slices in the search, allowing a greater degree of parallelism..
I also need to check that each variable is in the proper range during the search.
As an optimization, I check if each variable is less than `512`; the proof works similarly (`6656 < 8191`).

Because the search space is quite large, I parallelize it over multiple instances of the searcher. 
Each solver is given an offset and a stride over the valid range of the first free variable.
I instantiated 32 solvers, reducing the number of cycles by over 20x on my set of test cases.
I choose a power of two stride, because multiplying by a power of 2 modulo 8191 is a simple circular bit shift.

### Day 2
This problem asks us to compute the number of n-repeats in a list of ranges.

I approached this by first implementing a circuit for a fixed number of digits and repeats.
That circuit only requires a fixed number of divisions by a constant and multiplications.
Because the range for each solver is bounded by powers of 10, I implemented my own function to precompute the multiply-shift values to take advantage of those bounds.

For part 1, which asks for only 2-repeats, I simply instantiated separate instances for each digit count up to the maximum, and computed the sum.

For part 2, I instantiate separate instances for each digit count and divisor of that digit count.
However, I need to avoid overcounting by performing inclusion-exclusion on the divisors.
I apply a Mobius transform on the inclusion exclusion calculation to simplify the pre-computation of the coefficients.
My solution allows the user to set an arbitrary maximum digit count during generation, and will compute the coefficients on the fly.

The output is a continuous stream, with a 7 cycle delay.

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

For Part 2, I sort the ranges by their start as they come in, using parallelized insertion sort.
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

### Day 8, Part 2
While the problem description implies an edge-centric approach (Kruskal's algorithm), sorting `O(N**2)` edges is inefficient in hardware.
Finding the last edge connected in Kruskal's algorithm is equivalent to finding the longest edge in the minimum spanning tree.
Therefore, to improve the parallelism, I used Primm's algorithm, while tracking the largest edge added so far.

I fully parallelize the distance computations, determining a new edge in the MST in a fixed number of clock cycles.
This circuit runs in `O(n)` cycles, instead of the typical `O(n^2)` cycles that a Kruskal's algorithm approach might use.
This approach represents a significant area vs latency tradeoff, using a very large number of DSP slices to minimize total runtime.
If fewer DSP slices are available, we can instead process the edges in batches, maximizing the DSP slice usage while fitting on a more reasonably sized board.

To reduce routing congestion, the points are stored both in registers and BRAM.
The instance in registers is used for the parallel distance calculation, while the instance in BRAM is used for retrieving the coordinates of the newly added point.

### Day 9, Part 1
I read the points into a shift buffer, then compare new points in parallel against the previous points.

I observe that the test case and examples are simple orthogonal polygons, with points given in clockwise order and the edges alternating between horizontal and vertical.

Using this information, I eliminate roughly half the points from consideration by determining the interior angle.
The maximum area rectangle must be formed by points on the convex hull, and for a simple orthogonal polygon, points on the convex hull must have an interior angle of 90 degrees.

### Day 12
At first, this problem appears to be a grid packing problem that will require an ILP / constraint satisfaction approach.
However, the test cases provided by Advent of Code are trivial.
Either there are too many individual cells required to fit in the grid, or the grid can fit a number of 3x3 bounding boxes greater than or equal to the number of tiles.
The solution solves each test case in a constant number of cycles.

## Remaining Questions

### Day 8, Part 1
This question provides a list of points. It asks to compute the product of the sizes of the 3 largest connected components, after connecting the 1000 closest pair of points.

We can compute the distance for large batches of pairs per cycle, and only insert them into a 1000 element sorted array if they are less than the current largest distance in the array.
For computing the sizes of the connected components, we can use union-find, with union by size.

If we assume that the distances between pairs are independent and identically distributed, the probability of the `n`-th pair being inserted is roughly `1000 / n`.
This reduces the number of insertions from `1000 ** 2 / 2` to (in expectation) `1000 * log2(1000) < 10000`.

### Day 9, Part 2
This question asks to compute the largest (by area) axis aligned rectangle on a grid, where the rectangle has two opposite corners that are vertices of an orthogonal polygon and contains only cells that are contained by the polygon.

We can make the following observations: the polygon is simple, the edges are provided in clockwise order, and there are no adjacent edges.

The last observation is particularly useful.
Each edge is not a 1D line, but a 2D area on the grid.
Consider a polygon that snakes back and forth, filling a rectangular area.
The largest area rectangle would be the hull, even though it contains multiple 'edges'.
The last observation prevents that situation.

Because of those properties of the inputs, to determine if a rectangle is valid, it is sufficient to check if any edge intersects the rectangle and the corners face towards the interior.

We can simply iterate through all pairs of vertices, quickly checking if the corners face towards the interior and if the area is larger than the current candidate.
Then, if a pair passes the quick checks, pass it to a parallel checker that checks against all edges in parallel.

### Day 11
This question asks to compute the number of paths in a sparse directly acyclic graph (DAG).
This can be solved by a memoized DFS or a topological sort.
It is unlikely for a solution in an FPGA to be faster than an equivalent CPU implementation.
