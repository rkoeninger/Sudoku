{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

import Data.Set (Set, difference, fromList, singleton, size, unions, toList)
import Data.List (nub)
import Data.Maybe (catMaybes, listToMaybe, fromJust)

singleMaybe :: Set a -> Maybe a
singleMaybe s
	| isDecided s = listToMaybe (toList s)
	| otherwise = Nothing

next3 n = [n .. (n + 2)]
div3 n = (div n 3) * 3
mod3 n = (mod n 3) * 3

data Value = A | B | C | D | E | F | G | H | I deriving (Eq, Ord, Show, Read)
listValues = [A, B, C, D, E, F, G, H, I]
allValues = fromList listValues
numValues = size allValues

type Cell = Set Value
isDecided = (1 ==) . size

decidedValues :: [Cell] -> Set Value
decidedValues cells = fromList (catMaybes (map singleMaybe cells))

allIndicies = [0 .. (numValues - 1)]
gridCoords = map (zipWith ($) (map (flip (,)) allIndicies) . replicate 9) allIndicies
relatedCoords p@(r, c) = nub $ filter (/= p) (row ++ col ++ box)
	where row = map (r,) allIndicies
	      col = map (,c) allIndicies
	      box = [(x, y) | x <- next3 (div3 r), y <- next3 (div3 c)]

type Grid = [[Cell]]
zipWithCoords = zipWith zip gridCoords
getAt grid (r, c) = (grid !! r) !! c

reduceCell :: [Cell] -> Cell -> Cell
reduceCell otherCells cell = cell `difference` (decidedValues otherCells)

deduceCell :: [Cell] -> Cell -> Cell
deduceCell otherCells cell = if isDecided exclusiveValues then exclusiveValues else cell
	where exclusiveValues = cell `difference` (unions otherCells)

opGrid :: ([Cell] -> Cell -> Cell) -> Grid -> Grid
opGrid f grid = (map . map) opCell (zipWithCoords grid)
	where opCell (rc, cell) = f (map (getAt grid) (relatedCoords rc)) cell

reduceGrid = opGrid reduceCell
deduceGrid = opGrid deduceCell

reduceDeduce :: Grid -> Grid
reduceDeduce grid = result
	where nextGrid = deduceGrid (reduceGrid grid)
	      result | grid == nextGrid = grid
	             | otherwise = reduceDeduce nextGrid

branch :: Grid -> [Grid]
branch = error "not implemented"



-- Test Code

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

testGrid0 = asValues [[0,0,7,1,8,0,0,4,0],[0,0,4,0,0,0,8,0,0],[8,0,0,0,0,6,2,5,0],[0,4,0,0,9,8,7,1,0],[1,0,0,3,0,5,0,0,6],[0,9,5,2,1,0,0,8,0],[0,2,1,7,0,0,0,0,8],[0,0,3,0,0,0,4,0,0],[0,8,0,0,2,9,1,0,0]]
