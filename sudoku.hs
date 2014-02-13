{-# LANGUAGE ScopedTypeVariables #-}

import Data.Set (Set, difference, unions, fromList, findMin, notMember, singleton, size)
import qualified Data.Set (filter)
import Data.List (nub)
import Data.List.Split (chunksOf)

type Cell = Set Int
type Grid = [[Cell]]
type Coord = (Int, Int)

allPossibles = fromList [1..9]
allIndicies = [0..8]
gridCoords = map (\i -> map (\j -> (i, j)) [0..8]) [0..8]
allCoords = [(r, c) | r <- allIndicies, c <- allIndicies]
blankGrid = replicate 9 (replicate 9 allPossibles)
setupGrid :: [[Int]] -> Grid = map (map (\x -> if x == 0 then allPossibles else singleton x))

skipOver :: [a] -> Int -> [a]
skipOver list index = (take index list) ++ (drop (index + 1) list)

setNth :: Int -> a -> [a] -> [a]
setNth index value list = (take index list) ++ [value] ++ (drop (index + 1) list)

setCoord :: Coord -> a -> [[a]] -> [[a]]
setCoord (r, c) value matrix = setNth r (setNth c value (matrix !! r)) matrix

updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth index f list = setNth index (f $ list !! index) list

updateCoord :: Coord -> (a -> a) -> [[a]] -> [[a]]
updateCoord coord f matrix = setCoord coord (f $ getAt matrix coord) matrix

hasDup :: (Eq a) => [a] -> Bool
hasDup xs = length xs /= (length . nub) xs

isDecided :: Cell -> Bool = (== 1) . size
isExhausted :: Cell -> Bool = (== 0) . size
isSolved :: Grid -> Bool = (all isDecided) . getAll

getAll :: [[a]] -> [a]
getAll = concat

getAt :: [[a]] -> Coord -> a
getAt grid (r, c) = (grid !! r) !! c

getRow :: [[a]] -> Int -> [a]
getRow = (!!)

getCol :: [[a]] -> Int -> [a]
getCol grid index = map (!! index) grid

getBox :: [[a]] -> Int -> [a]
getBox grid index = (slice div grid) >>= (slice mod)
    where slice f list = take 3 (drop ((f index 3) * 3) list)

transformCoord :: (Int -> Int -> Int) -> Coord -> Int
transformCoord f (r, c) = (f r 3) * 3 + (f c 3)

getBoxCoord :: [[a]] -> Coord -> [a]
getBoxCoord grid coord = getBox grid (transformCoord div coord)

getRowExcept grid (r, c) = (getRow grid r) `skipOver` c
getColExcept grid (r, c) = (getCol grid c) `skipOver` r
getBoxExcept grid coord = (getBoxCoord grid coord) `skipOver` (transformCoord mod coord)

{- returns duplicates -}
getRelated :: [[a]] -> Coord -> [a]
getRelated grid coord = (\f -> f grid coord) =<< [getRowExcept, getColExcept, getBoxExcept]

getDecidedList :: [Cell] -> [Int]
getDecidedList cells = map findMin (Prelude.filter isDecided cells)

getDecidedSet = fromList . getDecidedList

checkGrid :: Grid -> Bool
checkGrid grid = all check [getRow, getCol, getBox]
    where check f = all (not . hasDup . getDecidedList) (map (f grid) allIndicies)

validate :: Grid -> Bool
validate grid = (all (not . isExhausted) (getAll grid)) && (checkGrid grid)

reduceCell :: Grid -> Coord -> Cell
reduceCell grid coord = (getAt grid coord) `difference` (getDecidedSet (getRelated grid coord))

deduceCell :: Grid -> Coord -> Cell
deduceCell grid coord = strip (getAt grid coord)
    where related = getRelated grid coord
          vals = Data.Set.filter (\val -> all (notMember val) related) (getAt grid coord)
          strip cell = if 0 == (size vals) then cell else singleton (findMin vals)

reduce :: Grid -> Grid
reduce grid = (map . map) (reduceCell grid) gridCoords

deduce :: Grid -> Grid
deduce grid = (map . map) (deduceCell grid) gridCoords
