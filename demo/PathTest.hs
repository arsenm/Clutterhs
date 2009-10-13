
import Graphics.UI.Clutter

main :: IO ()
main = do
  clutterInit

  stage <- stageGetDefault

  path <- pathNew

  pathAddMoveTo path 15 15
  pathAddRelMoveTo path 30 0
  pathAddRelLineTo path 10 100
  pathAddRelCurveTo path 10 10 40 40 90 90
  pathAddMoveTo path 200 200
  pathAddClose path
  n <- pathGetNNodes path
  length <- pathGetLength path
  putStrLn $ "Number of nodes: " ++ Prelude.show n
  putStrLn $ "Path length: " ++ Prelude.show length

  --CHECKME: This list is supposed to be freed but I don't remember if I made that happen
  nodes <- pathGetNodes path

  tml <- timelineNew 3000
  alph <- alphaNewFull tml EaseInSine

  behav <- behaviourPathNew alph path

  rect <- rectangleNewWithColor (Color 0 0 255 200)
  containerAddActor stage rect
  actorSetSize rect 50 50

  behaviourApply behav rect

  putStrLn "Nodes: "
  mapM_ print nodes

  actorShowAll stage

  timelineStart tml

  clutterMain

