
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
  txt <- textNewWithText "Sans 10" str
  textSetLineWrap txt True
  set txt [textLineWrapMode := WrapWholeWords]
  containerAddActor group rec
  containerAddActor group txt
--containerLowerChild group rec txt
  actorLowerBottom rec
  actorSetAnchorPointFromGravity group GravityCenter

  set rec [ actorX := 0,
            actorY := 0,
            actorWidth := 120,
            actorHeight := 120 ]
  set txt [ actorX := 0,
            actorY := 0,
            actorWidth := 120,
            actorHeight := 120 ]

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


spinAlpha = do
  tml <- timelineNew 3000
  set tml [ timelineLoop := True ]
  alphaNewFull tml Linear


opacityAlpha = do
  tml <- timelineNew 3000
  set tml [ timelineLoop := True ]
  alphaNewFull tml EaseInOutCubic

-- make something bizarre happen when you win
makeItSpin actor = do
  x <- get actor actorX
  y <- get actor actorY
  stage <- stageGetDefault
  sx <- get stage actorWidth
  sy <- get stage actorHeight
  alpha <- spinAlpha
  bhv <- behaviourEllipseNew (Just alpha)
                             (round x)
                             (round y)
                             (round (sx - x))
                             (round (sy-y))
                             RotateCw
                             0
                             360

  alpha2 <- opacityAlpha
  bhv2 <- behaviourOpacityNew (Just alpha2) 20 255

  alphaGetTimeline alpha >>= timelineStart
  alphaGetTimeline alpha2 >>= timelineStart
  behaviourApply bhv actor
  behaviourApply bhv2 actor

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
  when res (putStrLn "GLORIOUS VICTORY"
            >> mapM_ makeItSpin (map fst (elems arr))
           )


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
  mapM_ (addTileToStage stage 140 ioArr) (indices arr)

  actorShowAll stage

  clutterMain

