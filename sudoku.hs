{-# LANGUAGE TupleSections #-}

import Data.Set (Set, fromList, toList, size, singleton, difference, unions)
import Data.List (nub)
import Data.Maybe (catMaybes, listToMaybe, fromJust)

data Value = A | B | C | D | E | F | G | H | I deriving (Eq, Ord, Show, Read)
listValues = [A, B, C, D, E, F, G, H, I]
allValues = fromList listValues
numValues = size allValues

type Cell = Set Value
isDecided = (1 ==) . size

count :: (a -> Bool) -> [a] -> Int
count f xs = length (filter f xs)

singleMaybe :: Set a -> Maybe a
singleMaybe s
	| isDecided s = listToMaybe (toList s)
	| otherwise = Nothing

decidedValues :: [Cell] -> Set Value
decidedValues cells = fromList (catMaybes (map singleMaybe cells))

box3 n = let j = (div n 3) * 3 in [j .. (j + 2)]

allIndicies = [0 .. (numValues - 1)]
gridCoords = map (zipWith ($) (map (flip (,)) allIndicies) . replicate 9) allIndicies
relatedGroups rc@(r, c) = map (filter (/= rc)) [row, col, box]
	where row = map (r,) allIndicies
	      col = map (,c) allIndicies
	      box = [(x, y) | x <- box3 r, y <- box3 c]
relatedCoords rc = nub $ concat $ relatedGroups rc
allGroups = rows ++ cols ++ boxes
	where rows = gridCoords
	      cols = map (zipWith ($) (map (,) allIndicies) . replicate 9) allIndicies
	      boxes = map (\i -> [(x, y) | x <- box3 ((div i 3) * 3), y <- box3 ((div i 3) * 3)]) allIndicies

type Grid = [[Cell]]
zipWithCoords = zipWith zip gridCoords

getAt :: Grid -> (Int, Int) -> Cell
getAt grid (r, c) = (grid !! r) !! c

reduceCell :: [Cell] -> Cell -> Cell
reduceCell otherCells cell = cell `difference` (decidedValues otherCells)

deduceCellGroup :: [Cell] -> Cell -> Cell
deduceCellGroup otherCells cell = if isDecided exclusiveValues then exclusiveValues else cell
	where exclusiveValues = cell `difference` (unions otherCells)

deduceCell :: Grid -> (Int, Int) -> Cell
deduceCell grid rc = foldr deduceCellGroup cell relatedCells
	where cell = getAt grid rc
	      relatedCells = (map . map) (getAt grid) (relatedGroups rc)

opGrid :: ([Cell] -> Cell -> Cell) -> Grid -> Grid
opGrid f grid = (map . map) opCell (zipWithCoords grid)
	where opCell (rc, cell) = f (map (getAt grid) (relatedCoords rc)) cell

reduceGrid = opGrid reduceCell

deduceGrid :: Grid -> Grid
deduceGrid grid = (map . map) (deduceCell grid) gridCoords

reduceDeduce :: Grid -> Grid
reduceDeduce grid = result
	where nextGrid = deduceGrid (reduceGrid grid)
	      result | grid == nextGrid = grid
	             | otherwise = reduceDeduce nextGrid

notEmpty :: Set a -> Bool
notEmpty = (/= 0) . size

isValid :: Grid -> Bool
isValid grid = noneEmpty && allValid
	where noneEmpty = all notEmpty (concat grid)
	      allValid = and (map (validateGroup . map (getAt grid)) allGroups)
	      validateGroup cells = unique (catMaybes (map singleMaybe cells))
	      unique xs = (length xs) == (length (nub xs))

isSolved :: Grid -> Bool
isSolved grid = valid && allDecided
	where valid = isValid grid
	      allDecided = all isDecided (concat grid)

branchCandidates :: [Cell] -> [Cell]
branchCandidates cells = filter moreThan1 cells
	where moreThan1 cell = 1 < (count (== cell) cells)

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

testGrid0 = asValues [[0,0,7,1,8,0,0,4,0],
                      [0,0,4,0,0,0,8,0,0],
                      [8,0,0,0,0,6,2,5,0],
                      [0,4,0,0,9,8,7,1,0],
                      [1,0,0,3,0,5,0,0,6],
                      [0,9,5,2,1,0,0,8,0],
                      [0,2,1,7,0,0,0,0,8],
                      [0,0,3,0,0,0,4,0,0],
                      [0,8,0,0,2,9,1,0,0]]

testGrid1 = asValues [[9,0,0,5,0,0,2,0,3],
                      [0,0,0,0,7,3,0,0,0],
                      [4,0,5,0,0,0,9,0,0],
                      [0,2,0,4,0,8,0,0,7],
                      [0,5,0,0,6,0,0,3,0],
                      [6,0,0,7,0,2,0,4,0],
                      [0,0,2,0,0,0,6,0,1],
                      [0,0,0,3,2,0,0,0,0],
                      [8,0,1,0,0,7,0,0,5]]

solveMaybe :: Grid -> Maybe [[Int]]
solveMaybe grid = fmap asInts (if isSolved grid_ then Just grid_ else Nothing)
	where grid_ = reduceDeduce grid

{-

testGrid_ = asValues [[0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0,0]]

-}
