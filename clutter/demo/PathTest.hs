-- This tests Path and BehaviourPath

import Prelude
import qualified Prelude as P
import Graphics.UI.Clutter
import System.Glib.Signals
import Control.Monad (when)
import System.IO


testForeachFunc :: PathCallback
testForeachFunc pn = putStr "PathCallback: " >> print pn


-- example from docs
--For example, to move an actor in a 100 by 100 pixel square
--centered on the point 300,300 you could use the following path:
testPathStr = "M 250,350 l 0 -100 L 350,250 l 0 100 z"

testPathDescript :: IO Path
testPathDescript = do
  path <- pathNew
  ret <- pathAddString path testPathStr
  putStrLn $ "pathAddString: " ++ if ret then "Success" else "Failure"
  return path

testPathNewDescript :: IO Path
testPathNewDescript = pathNewWithDescription testPathStr

testPathAddMoves :: IO Path
testPathAddMoves = do
  path <- pathNew

  pathAddMoveTo path 15 15
  pathAddRelMoveTo path 30 0
  pathAddRelLineTo path 10 100
  pathAddRelCurveTo path 10 10 40 40 90 90
  pathAddRelCurveTo path 50 30 40 40 140 90
  pathAddMoveTo path 300 350
  pathAddRelCurveTo path 10 20 50 40 40 90
  pathAddClose path

  return path


main :: IO ()
main = do
  clutterInit

  stage <- stageGetDefault

  --Path1 tests adding moves
  path1 <- testPathAddMoves
  n1 <- pathGetNNodes path1
  len1 <- pathGetLength path1
  --CHECKME: This list is supposed to be freed but I don't remember if I made that happen
  nodes1 <- pathGetNodes path1
  putStrLn $ "Path1: Number of nodes: " ++ P.show n1
  putStrLn $ "Path1:  length: " ++ P.show len1
  pos1 <- pathGetPosition path1 0.5
  putStrLn $ "Path1: pathGetPosition: pos = " ++ P.show pos1
  putStrLn "Path1 Nodes: "
  mapM_ print nodes1
  descr <- pathGetDescription path1      -- see if the string description is correct
  putStrLn ("Path1 Description: " ++ descr)

--Path2 tests using string descriptions
  path2 <- testPathDescript
  n2 <- pathGetNNodes path2
  len2 <- pathGetLength path2
  putStrLn $ "Path2: Number of nodes: " ++ Prelude.show n2
  putStrLn $ "Path2:  length: " ++ Prelude.show len2


--Path3 should be the same as path2, i.e. they should track
  path3 <- testPathNewDescript
  n3 <- pathGetNNodes path3
  len3 <- pathGetLength path3
  putStrLn $ "Path3: Number of nodes: " ++ Prelude.show n3
  putStrLn $ "Path3:  length: " ++ Prelude.show len3

  when ( n2 /= n3 || len3 /= len2 ) (hPutStrLn stderr "Path2 /= Path3!")

  pathForeach path1 testForeachFunc

  tml <- timelineNew 3000
  alph <- alphaNewFull tml EaseInSine

  behav1 <- behaviourPathNew alph path1
  behav2 <- behaviourPathNew alph path2
  behav3 <- behaviourPathNew alph path3
  behav4 <- behaviourPathNewWithDescription alph testPathStr
--  behav5 <- behaviourPathNewWithKnots alph [ (250,350), (0,(-100)), (350,250), (0,10) ]
  behav5 <- behaviourPathNewWithKnots alph [ (100,100), (250, 250), (300, 300), (200, 200) ]


  --rect[1..4] should all move together
  rect1 <- rectangleNewWithColor (Color 0 0 255 200)
  rect2 <- rectangleNewWithColor (Color 0 255 0 150)
  rect3 <- rectangleNewWithColor (Color 255 0 0 150)
  rect4 <- rectangleNewWithColor (Color 0 123 123 150)
  rect5 <- rectangleNewWithColor (Color 123 200 123 150)

  mapM_ (containerAddActor stage) [rect1, rect2, rect3, rect4, rect5]
  actorSetSize rect1 50 50
  actorSetSize rect2 20 60
  actorSetSize rect3 60 20
  actorSetSize rect4 40 40
  actorSetSize rect5 50 50

  actorSetPosition rect2 200 100
  actorSetPosition rect3 150 100
  actorSetPosition rect5 300 200

  behaviourApply behav1 rect1
  behaviourApply behav2 rect2
  behaviourApply behav3 rect3
  behaviourApply behav4 rect4
  behaviourApply behav5 rect5

  behav5 `on` knotReached $ \n -> putStrLn ("Path5: Knot " ++ P.show n ++ " reached")

  actorShowAll stage

  timelineStart tml

  on tml completed mainQuit

  clutterMain

