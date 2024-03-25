module Main where

import qualified GlossMain as GM
import qualified BrickMain as BM

main = do
    -- First ask user for file they want to run
    putStrLn "Enter the filename: "
    fileName <- getLine
    -- Read in the file from the resources folder
    fileContents <- readFile ("resources/" ++ fileName ++ ".txt")
    -- Split the file into lines, making sure to remove the size line
    let fileLines = tail (lines fileContents)
    -- Now ask the user if they want to run gloss or brick
    putStrLn "Do you want to run with gloss or brick?"
    ui <- getLine
    if ui == "gloss"
    then GM.run fileLines
    else BM.run fileLines