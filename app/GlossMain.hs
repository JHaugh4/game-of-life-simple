module GlossMain (run) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort

import Data.List

import GOL

-- Now to display our game of life board
-- we need to know a bit about gloss
-- First gloss uses the convention of (0, 0) being
-- at the center of the screen.
-- Second you construct gloss shapes using composition
-- Third our goal function to implement is the simulate
-- function. This function basically wants the initial
-- state of the screen, a function to turn the state
-- into an image, and a function to advance the state.

-- We will display our cells as circles which are either
-- green when alive or red when dead. Let's write a function
-- to convert the Ints 0 or 1 to these circles. We will
-- also need to know the size of the cells
cellToPicture :: Float -> Int -> Picture
cellToPicture cs c = 
    case c of
        0 -> color red   (circle cs)
        1 -> color green (circle cs)
    
-- Normally you move picture with the translate function,
-- however, this function as I said before assumes the origin
-- is in the middle of the screen. It would be nice to instead
-- have a translate which thinks the origin is at the top left
-- of the screen and that x is positive to the right and y is
-- positive going down.
translate' :: Float -> Float -> Float -> Picture -> Picture
translate' ws x y = translate (x - halfWS) ((-y) + halfWS)
    where
        halfWS = ws / 2

-- Now a function to convert an entire grid to a picture
-- It will need to know the size of the screen
gridToPicture :: Float -> [[Int]] -> Picture
gridToPicture ws grid = pictures (concat ((map . map) go (labelGrid grid)))
    where
        gs = fromIntegral (length grid)
        cs = ws / gs
        go ((x, y), c) =
            let cx = (fromIntegral x * cs) + (cs / 2)
                cy = (fromIntegral y * cs) + (cs / 2)
            in translate' ws cx cy (cellToPicture (cs / 2) c)

-- Now the last thing we need is to define the function to advance
-- the state
step :: ViewPort -> Float -> [[Int]] -> [[Int]]
step vp s = evolve

-- Now we need just need to piece it all together
run :: [String] -> IO ()
run fileLines = do
    -- Create the grid
    let grid = readGrid fileLines
    -- Ask for the window size
    putStrLn "Enter the desired window size: "
    windowSizeStr <- getLine
    let ws = read windowSizeStr
    -- Create the window
    let window = InWindow "Game of Life" (ws, ws) (0, 0)
    -- Now we can simulate
    let wsF = fromIntegral ws
    simulate window black 15 grid (gridToPicture wsF) step