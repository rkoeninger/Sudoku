# sudoku

Sudoku solver written in Haskell. Based on something much uglier that I wrote in C#.

I've had to introduce some vocabulary:

"box" - One of the nine 3x3 sections on the grid.
"group" - A row, column or box.
"related cells" - The set all of the cells in the same row, column and box as a given cell, without the given cell.
"decided cell" - A cell is decided when it has been reduced to one possible value. That value is the "decided value".

Solves puzzles using two basic operations (and these probably need better names):

"reduce" - for each cell, find all other related cells that have been reduced to one possible value and remove those possible values from the current cell.

"deduce" - for each cell, if it has a value that no other related cell can be, make the current cell that value.

"Branching" will also have to be introduced for puzzles that aren't so easy. Sometimes a puzzle will reduce/deduce down to where there are two cells in the same group that have the same two possible values. In this case, the solver will have to branch on both possibilies (cellX=1, cellY=2 and cellX=2, cellY=1). Continue reducing both grids. One will likely be a dead end where a cell ends up with no possible values. The other will lead to the solution.

Here's a test grid and the solution the solver comes up with and the "validate" function returns true for:

[[0,0,7,1,8,0,0,4,0],
 [0,0,4,0,0,0,8,0,0],
 [8,0,0,0,0,6,2,5,0],
 [0,4,0,0,9,8,7,1,0],
 [1,0,0,3,0,5,0,0,6],
 [0,9,5,2,1,0,0,8,0],
 [0,2,1,7,0,0,0,0,8],
 [0,0,3,0,0,0,4,0,0],
 [0,8,0,0,2,9,1,0,0]]

[[5,3,7,1,8,2,6,4,9],
 [2,6,4,9,5,3,8,7,1],
 [8,1,9,4,7,6,2,5,3],
 [3,4,2,6,9,8,7,1,5],
 [1,7,8,3,4,5,9,2,6],
 [6,9,5,2,1,7,3,8,4],
 [9,2,1,7,3,4,5,6,8],
 [7,5,3,8,6,1,4,9,2],
 [4,8,6,5,2,9,1,3,7]]

The zeroes in the input grid get translated into a set of all nine possible values before solving begins. The non-zero values are translated to single-value sets.