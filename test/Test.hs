module Main where

import System.Exit (exitSuccess, exitFailure)
import Data.Set (singleton, size)
import Data.Maybe (fromJust)
import Sudoku

asValues :: [[Int]] -> Grid
asValues vals = (map . map) interpretVal vals
	where interpretVal x = case x of
		0 -> allValues
		_ -> singleton (listValues !! (x - 1))

asInts :: Grid -> [[Int]]
asInts grid = (map . map) interpretVal grid
	where interpretVal x = case fromJust (singleMaybe x) of
		A -> 1
		B -> 2
		C -> 3
		D -> 4
		E -> 5
		F -> 6
		G -> 7
		H -> 8
		I -> 9

singleSolution = asValues
	[[9,0,0,5,0,0,2,0,3],
     [0,0,0,0,7,3,0,0,0],
     [4,0,5,0,0,0,9,0,0],
     [0,2,0,4,0,8,0,0,7],
     [0,5,0,0,6,0,0,3,0],
     [6,0,0,7,0,2,0,4,0],
     [0,0,2,0,0,0,6,0,1],
     [0,0,0,3,2,0,0,0,0],
     [8,0,1,0,0,7,0,0,5]]

multipleSolutions = asValues
    [[9,0,0,5,0,0,2,0,3],
     [0,0,0,0,7,3,0,0,0],
     [0,0,5,0,0,0,9,0,0],
     [0,2,0,4,0,8,0,0,7],
     [0,5,0,0,6,0,0,3,0],
     [6,0,0,7,0,2,0,0,0],
     [0,0,2,0,0,0,6,0,1],
     [0,0,0,3,2,0,0,0,0],
     [8,0,1,0,0,7,0,0,5]]

noSolutions = asValues
    [[9,0,0,5,0,0,2,0,3],
     [0,0,0,0,7,3,0,0,0],
     [0,0,5,0,0,0,9,0,0],
     [0,2,0,4,0,8,0,0,7],
     [0,5,0,0,6,0,0,3,0],
     [6,0,0,7,0,2,0,0,0],
     [0,1,2,0,0,0,6,0,1],
     [0,0,0,3,2,0,0,0,0],
     [8,0,0,0,0,7,0,0,5]]

{-

testGrid_ = asValues
	[[0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0]]

-}

scenarios = [
	(singleSolution, 1),
	(multipleSolutions, 17),
	(noSolutions, 0)]
testScenario grid = (==) (size (solve grid))
test = all (uncurry testScenario) scenarios

main = do
	putStrLn ("Testing " ++ show (length scenarios) ++ " scenarios...")
	let returnCode = if test then exitSuccess else exitFailure
	putStrLn "Tests complete."
	returnCode
