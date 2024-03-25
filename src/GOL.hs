module GOL (evolve, readGrid, labelGrid) where

import Data.Char (digitToInt, intToDigit)

-- Our goal is to define this function
-- We are using Int to represent our cell states
-- evolve :: [[Int]] -> [[Int]]]

-- What are the rules of game of life?
-- Well they rely on the number of alive neighbors of a cell
-- and the value of the cell itself. Let's write
-- this function as if we have the information
-- and worry about gathering it later:
rules :: Int -> Int -> Int
rules liveNeighbors 1
    | liveNeighbors == 2 || 
      liveNeighbors == 3 = 1
    | otherwise          = 0
rules liveNeighbors 0
    | liveNeighbors == 3 = 1
    | otherwise          = 0
    
-- Let's first make a helper function to read in a grid
-- from a list of Strings
readGrid :: [String] -> [[Int]]
readGrid = (map . map) digitToInt

showGrid :: [[Int]] -> [String]
showGrid = (map . map) intToDigit

-- Test
testGridStr :: [String]
testGridStr =
    [
        "0000000000",
        "0000000000",
        "0000000000",
        "0000111000",
        "0000000000",
        "0000000000",
        "0000010000",
        "0000010000",
        "0000010000",
        "0000000000"
    ]

testReadShow :: Bool
testReadShow = (showGrid . readGrid) testGridStr == testGridStr

-- Now we need a way to talk about coordinates in our grid
-- I will use x and y to denote column and row respectively.
-- We will represent coordinates as tuples of Ints
-- It will also be useful to add coordinates together in order
-- to find coordinates offset by some values. Thus, let's make
-- a function to do this:
addCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Test it
testCoord1 :: (Int, Int)
testCoord1 = (2, 3)

testCoord2 :: (Int, Int)
testCoord2 = (-1, 0)

testAddCoords :: Bool
testAddCoords = addCoords testCoord1 testCoord2 == (1, 3)

-- Okay now we have to tackle the problem of getting the 
-- neighborhood of a cell. There are at least two ways
-- we can think about our board:
-- 1. Circular (wrap around on the edges)
-- 2. Infinite (out of bounds is just filled with 0s)
-- Let's say we wanted it to be infinite then we need a function
-- which when asked for an out of bounds point returns 0.
-- Ask yourself should the order of arguments be point then grid
-- or grid then point? Which is more likely to vary? Probably
-- the point so it will go last. You will see why this is
-- useful later.
getInfinite :: [[Int]] -> (Int, Int) -> Int
getInfinite grid (x, y)
    | x < 0     || y < 0 || 
      x >= size || y >= size = 0
    | otherwise        = grid !! y !! x
    where
        size = length grid

-- Test
testGrid :: [[Int]]
testGrid = readGrid testGridStr

testGetInfinite1 :: Bool
testGetInfinite1 = getInfinite testGrid (4, 3) == 1

testGetInfinite2 :: Bool
testGetInfinite2 = getInfinite testGrid (-1, 0) == 0

testGetInfinite3 :: Bool
testGetInfinite3 = getInfinite testGrid (5, 10) == 0

-- Now what if we wanted to instead have a circular grid?
-- When we go out of bounds we now wrap around
-- First not that the - sign in Haskell behaves
-- a bit strangely. Try this in ghci:

-- >>> -1 `mod` 10
-- -1

-- vs

-- >>> (-1) `mod` 10
-- 9

-- The first case is really this:
-- >>> -(1 `mod` 10)
-- -1

-- So be careful!

-- This shows thought that mod is exactly what we
-- want when a coordinate goes out of bounds in either
-- direction
getCircular :: [[Int]] -> (Int, Int) ->  Int
getCircular grid (x, y)
    | x < 0     || y < 0 || 
      x >= size || y >= size = grid !! (y `mod` size) !! (x `mod` size)
    | otherwise        = grid !! y !! x
    where
        size = length grid

-- Test
testGetCircular1 :: Bool
testGetCircular1 = getCircular testGrid (4, 3) == 1

testGetCircular2 :: Bool
testGetCircular2 = getCircular testGrid (5, -2) == 1

testGetCircular3 :: Bool
testGetCircular3 = getCircular testGrid (-4, 3) == 1

testGetCircular4 :: Bool
testGetCircular4 = getCircular testGrid (-1, -1) == 0

-- Okay now onto neighborhoods
-- Using the idea of offsets from a point we can quite
-- easily get the neighborhood of a given point. This
-- function will also take the function to use as the 
-- getter function
getNeighborhood :: ([[Int]] -> (Int, Int) -> Int) -> [[Int]] -> (Int, Int) -> [Int]
getNeighborhood get grid (x, y) = map (get grid) neighborCoords
    where
        offsets = [ -1, 0, 1 ]
        neighborCoords =
            [ addCoords (x, y) (xo, yo) | xo <- offsets, yo <- offsets, (xo, yo) /= (0, 0) ]

-- Test
testGetNeighborhood1 :: Bool
testGetNeighborhood1 =
    getNeighborhood getCircular testGrid (4, 3) == [0, 0, 0, 0, 0, 0, 1, 0]

testGetNeighborhood2 :: Bool
testGetNeighborhood2 =
    getNeighborhood getInfinite testGrid (4, 3) == [0, 0, 0, 0, 0, 0, 1, 0]


-- Now we need a function which can take a grid and add coordinates
-- to each cell in it
labelGrid :: [[Int]] -> [[((Int, Int), Int)]]
labelGrid grid =
    [ [ ((x, y), cell) | (x, cell) <- zip [0..] row ] | (y, row) <- zip [0..] grid ]

-- Test
testLabelGrid :: Bool
testLabelGrid = labelGrid grid == ans
    where
        grid = [[0,1,2], 
                [3,4,5], 
                [6,7,8]]
        ans  = [[((0,0),0),((1,0),1),((2,0),2)],
                [((0,1),3),((1,1),4),((2,1),5)],
                [((0,2),6),((1,2),7),((2,2),8)]]

-- Now we can finally write evolve function from above
evolve :: [[Int]] -> [[Int]]
evolve grid = (map . map) go (labelGrid grid)
    where
        go (coord, cell) =
            let liveNeighbors = sum (getNeighborhood getCircular grid coord)
            in rules liveNeighbors cell

-- Test
testEvolve :: Bool
testEvolve = evolve testGrid == ans
    where
        ans = [[0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,1,0,0,0,0],
               [0,0,0,0,0,1,0,0,0,0],
               [0,0,0,0,0,1,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,1,1,1,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0]]

runTests :: Bool
runTests = and 
    [
        testReadShow,
        testAddCoords,
        testGetInfinite1,
        testGetInfinite2,
        testGetInfinite3,
        testGetCircular1,
        testGetCircular2,
        testGetCircular3,
        testGetCircular4,
        testGetNeighborhood1,
        testGetNeighborhood2,
        testLabelGrid,
        testEvolve
    ]

-- data Cell = Dead | Alive
--     deriving (Eq, Show)

-- type Grid a = [[a]]

-- gridSize = 100

-- evolve :: Grid Cell -> Grid Cell
-- evolve grid = (fmap . fmap) go (labelGrid grid)
--     where
--         go (p, c) = rule (getNeighbors grid p) c

-- readGrid :: [[Char]] -> [[Cell]]
-- readGrid = (fmap . fmap) cellFromChar
--     where
--         cellFromChar '0' = Dead
--         cellFromChar '1' = Alive

-- type Point = (Int, Int)

-- (+++) :: Point -> Point -> Point
-- (x1, y1) +++ (x2, y2) = (xsum, ysum)
--     where
--         xsum = (x1 + x2) `mod` gridSize
--         ysum = (y1 + y2) `mod` gridSize

-- mapGrid :: (a -> b) -> Grid a -> Grid b
-- mapGrid f grid = fmap (fmap f) grid

-- labelGrid :: Grid a -> Grid (Point, a)
-- labelGrid grid =
--     [ [ ((x, y), a) | (x, a) <- zip [0..] as ] | (y, as) <- zip [0..] grid ]

-- getCell :: Grid Cell -> Point -> Cell
-- getCell grid (x, y) = grid !! y !! x

-- neighborPoints :: [Point]
-- neighborPoints =
--     [ (x, y) | x <- [-1, 0, -1], y <- [-1, 0, -1], (x, y) /= (0, 0) ]

-- getNeighbors :: Grid Cell -> Point -> [Cell]
-- getNeighbors grid p =
--     fmap (getCell grid . (+++ p)) neighborPoints

-- rule :: [Cell] -> Cell -> Cell
-- rule neighbors c = case c of
--     Dead ->
--         if numLiveNeighbors == 3
--         then Alive
--         else Dead
--     Alive ->
--         if numLiveNeighbors == 2 ||
--            numLiveNeighbors == 3
--         then Alive
--         else Dead
--     where
--         liveNeighbors = filter (== Alive) neighbors
--         numLiveNeighbors = length liveNeighbors
