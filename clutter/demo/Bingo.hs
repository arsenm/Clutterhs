
-- kind of crappy demo of a simple bingo game. Maybe not a good
-- example since it's terrible but whatever.

import Prelude
import qualified Prelude as P

import Graphics.UI.Clutter
import System.Glib.Attributes
import System.Glib.Signals

import System.Random.Shuffle
import System.Environment

import Control.Monad.Trans (liftIO)
import Control.Monad
import Data.Maybe
import Random

import Data.Array
import Data.IORef

isqrt = floor .  sqrt . fromIntegral

createTile ioArr str = do
  group <- groupNew
  rec <- rectangleNewWithColor (fromJust $ colorFromString "blue")
  txt <- textNewWithText "Sans 20" str
  containerAddActor group rec
  containerAddActor group txt
--containerLowerChild group rec txt
  actorLowerBottom rec
  actorSetAnchorPointFromGravity group GravityCenter

  set rec [ actorX := 0,
            actorY := 0,
            actorWidth := 75,
            actorHeight := 75 ]
  set txt [ actorX := 0,
            actorY := 0,
            actorWidth := 75,
            actorHeight := 75 ]

  actorSetReactive group True
  on group buttonPressEvent $
    tryEvent $
      liftIO $ do arr <- readIORef ioArr
                  Just name <- get group actorName
                  let index = read name
                  let (tile, stat) = arr ! index
                  if stat
                     then set rec [ rectangleColor := fromJust (colorFromString "blue") ]
                          >> modifyIORef ioArr ( // [(index, (tile, False))] )
                     else set rec [ rectangleColor := fromJust (colorFromString "red") ]
                          >> modifyIORef ioArr ( // [(index, (tile, True))] )
                  checkWon ioArr
  return group

checkWon ioArr = do
  arr <- readIORef ioArr
  -- assumes square matrix
  let ((lb, _), (ub, _)) = bounds arr
  let rng = [lb..ub]

  --check each column
  let a = [ and [ snd $ arr ! (x, y) | y <- rng ] | x <- rng]
  --and each row
  let b = [ and [ snd $ arr ! (x, y) | x <- rng ] | y <- rng]
  -- and the diagonals
  let c = and [ snd  $ arr ! (x, x) | x <- rng ]
  let d = and [ snd $ arr ! (x, ub - x + 1) | x <- rng ]

  let res = or [or a, or b, c ,d]
  when res (putStrLn "GLORIOUS VICTORY")


addTileToStage stage spacing ioTiles p@(x,y) = do
  tiles <- readIORef ioTiles
  let tile = fst (tiles ! p)
  containerAddActor stage tile
  set tile [ actorX := (fromIntegral x) * spacing,
             actorY := (fromIntegral y) * spacing,
             actorName := Just (P.show p) ]


main = do
  clutterInit
  stage <- stageGetDefault

  set stage [ actorWidth := 800,
              actorHeight := 800 ]

  args <- getArgs
  items <- fmap lines (readFile (head args))
  let l = length items
  let side = isqrt l    -- just use first perfect square
  shufList <- fmap (shuffle' items l . mkStdGen) randomIO

  --This ioref usage is somewhat terrible

  ioArr <- newIORef (undefined :: Array (Int, Int) (Group, Bool))
  tiles <- mapM (createTile ioArr) shufList
  let arr = listArray ((1,1), (side,side)) (zip tiles (repeat False))
  writeIORef ioArr arr

-- Apparently can't use Uarray with Actors. Maybe fix this. So use
-- silly IORef.
  mapM_ (addTileToStage stage 120 ioArr) (indices arr)

  actorShowAll stage

  clutterMain

