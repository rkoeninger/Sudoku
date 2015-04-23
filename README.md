[![Build Status](https://travis-ci.org/rkoeninger/sudoku.svg?branch=master)](https://travis-ci.org/rkoeninger/sudoku)

# Sudoku Solver

Branching sudoku solver written in Haskell. Given an input grid, this program will find all possible solutions or determine if no solution exists.

## Problem Description

A sudoku puzzle is modeled as a grid with a set of remaining possible values in each cell. When a puzzle is loaded, each of the cells with an initial value has its set of possible values reduced to only that initial value. The cells with no value provided are populated with all nine possible values.

A grid is structured in 27 groups: nine rows, nine columns and nine 3x3 boxes. Each cell is a member of 3 groups: one row, one column and one box. The related cells of `x` are all the cells in all of `x`'s groups besides `x`. Each cell has exactly 20 related cells.

A solved grid has only one possible value remaining in each cell and no value appears more than once in any group.

## Solution Approach

Given the invariants of a sudoku puzzle, a few operations can be used to reliably reduce the values in a cell:

  * Reduction - If there is a cell `x` that has been reduced to a single value, that value can be removed from all of `x`'s related cells.
  * Deduction - If there is a cell `x` that is the only cell in a group with a particular value, it can immediately reduced to that single value.

For some puzzles, the above operations will not completely reduce the grid to a solved state. For example, there could be two cells in the group which have the same two possible values remaining. In this case, the grid will be branched. The solver will progress in two directions (or however many directions are necessary) by arbitrarily setting one of the irreducible cells to a different value remaining for that cell. Then reduction/deduction can continue until the grid is solved or another branch must be made.

If a branches is reduced to an invalid state, it is abandoned. All branches that ultimately result in a valid, solved grid are solutions to the input grid.

A grid is invalid if any cells have been reduced to zero possible values or if any two cells in the same group have only the same, single possible value remaining. An invalid grid cannot progress to a solved state.

## Future Work

The solver currently does not have any kind of user interface. The code has to be directly edited to input a puzzle.

A better interface would need be needed for anyone who doesn't want to edit Haskell syntax. But what would really be impressive would be a mobile app that could OCR a printed puzzle from the newspaper or wherever and immediately display the solution(s).
