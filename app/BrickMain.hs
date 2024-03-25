module BrickMain (run) where

-- Doing your imports this way where you make which
-- functions you are importing explicit is tedious
-- but pays off as your code grows more and more.
-- This is because at a glance you can tell where
-- a certain function came from.
import Brick.Main (App(..), customMain, neverShowCursor)
import Brick.Types (Widget, BrickEvent(..), EventM, get, put)
import Brick.Widgets.Core (str)
import Brick.Widgets.Table (table, renderTable)
import Brick.BChan (newBChan, writeBChan)
import Brick.AttrMap (AttrMap, attrMap)

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)

import GOL

-- Similar to gloss we need to define a couple
-- key function:
-- 1. How do we draw our board
-- 2. How do we evolve our board
-- In brick this takes the form of the App datatype
-- which holds these functions among a few other things.
-- Let's start by defining our drawing function. For simplicity
-- I will be using a table with strings for each cell
drawCell :: Int -> Widget ()
drawCell = str . show

drawGrid :: [[Int]] -> [Widget ()]
drawGrid grid = [ renderTable tbl ]
    where
        tbl = table . (map . map) drawCell $ grid

-- Now brick is not setup by default to evolve
-- frame by frame like gloss. However, we can
-- set this up ourselves using a bit of opaque
-- magic. But first we need a datatype to represent
-- this.
data ClockTick = Tick
-- This datatype simply represents a new frame
-- being requested.
-- Now we can setup our function to handle this
-- event and ignore all the rest. To see what events
-- are available take a look at BrickEvent in Brick.Types.
handleEvent :: BrickEvent () ClockTick -> EventM () [[Int]] ()
handleEvent (AppEvent Tick) = do
    grid <- get
    put (evolve grid)
-- Any other event just do nothing
handleEvent bevent = return ()

-- Now lets create our App
theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App [[Int]] ClockTick ()
app = App
  { appDraw         = drawGrid
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

-- Now we can finally pull it all together
run :: [String] -> IO ()
run fileLines = do
    -- All of this is to allow brick to handle an
    -- event every delay amount of microseconds
    -- You won't really need to mess with this
    -- other than to change the delay
    let delay = 100000 -- 100 ms
    chan <- newBChan 10
    void . forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    --
    -- Create the grid
    let grid = readGrid fileLines
    -- Initialize the vty
    let builder = VCP.mkVty V.defaultConfig
    initialVty <- builder
    -- Finally start the TUI
    customMain initialVty builder (Just chan) app grid
    return ()