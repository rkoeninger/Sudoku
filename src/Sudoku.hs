{-# LANGUAGE TupleSections #-}

module Sudoku (
	module Sudoku
) where

import Data.Set (Set, fromList, toList, size, singleton, difference, unions)
import qualified Data.Set as S (map)
import Data.List (nub, minimumBy)
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Data.Function (on)

data Value = A | B | C | D | E | F | G | H | I deriving (Eq, Ord, Show, Read)
listValues = [A, B, C, D, E, F, G, H, I]
allValues = fromList listValues
numValues = size allValues

type Cell = Set Value
isDecided = (1 ==) . size

flatSet :: (Ord a) => Set (Set a) -> Set a
flatSet = unions . toList

singleMaybe :: Set a -> Maybe a
singleMaybe s
	| isDecided s = listToMaybe (toList s)
	| otherwise   = Nothing

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

replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

setAt :: Grid -> (Int, Int) -> Value -> Grid
setAt grid (r, c) val = replaceNth r (replaceNth c (singleton val) (grid !! r)) grid

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

compareBranchCells :: Cell -> Cell -> Ordering
compareBranchCells g1 g2 = if forgetG1 || forgetG2 then defaultG else order
	where forgetG1 = isDecided g1
	      forgetG2 = isDecided g2
	      defaultG = if forgetG1 then GT else LT
	      order = on compare size g1 g2

compareBranchCoords :: Grid -> (Int, Int) -> (Int, Int) -> Ordering
compareBranchCoords grid = compareBranchCells `on` (getAt grid)

findBranchCoords :: Grid -> (Int, Int)
findBranchCoords grid = minimumBy (compareBranchCoords grid) (concat gridCoords)

branch :: Grid -> Set Grid
branch grid = S.map assume (getAt grid branchCoords)
	where branchCoords = findBranchCoords grid
	      assume val = setAt grid branchCoords val

solve :: Grid -> Set Grid
solve grid = if isSolved grid' then singleton grid' else grids
	where grid' = reduceDeduce grid
	      grids = flatSet $ S.map solve (branch grid')
