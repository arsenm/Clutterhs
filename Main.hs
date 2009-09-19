
module Main where

import Graphics.UI.Clutter
import System.Mem

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  a <- clutterInit
  print a

  c <- colorNew 255 0 0 200
  c2 <- colorNew 0 0 123 123
  blue <- colorNew 0 0 255 200
  rec <- rectangleNewWithColor c
  rec2 <- rectangleNewWithColor blue
  stg <- stageNew
  txt <- textNewWithText "sans" "CLUTTER HASKELL LOL"

  containerAddActor stg rec
  containerAddActor stg rec2
  containerAddActor stg txt

  actorSetPosition rec 50 50
  actorSetSize rec 100 100

  actorSetPosition rec2 100 100
  actorSetSize rec2 100 100

  (xpos, ypos) <- actorGetPosition rec
  print xpos
  print ypos
--  stageSetColor stg c2

  actorSetPosition txt 100 200
  actorSetSize txt 100 30

  orig <- stageGetPerspective stg
  putStrLn $ "got orig: " ++ show orig


  let pers = Perspective 90 2 0.1 100

  putStrLn $ "setting to: " ++ show pers

  stageSetPerspective stg pers
--  performGC

  onShow stg (putStrLn "I'm shown!")
  onButtonPressEvent stg (\_ -> putStrLn "I'm clicked!")

  actorShow rec
  actorShow txt
  actorShow stg

  out <- stageGetPerspective stg
  putStrLn $ "got out: " ++ show out

  actorShowAll stg

  a <- actorGetZRotationGravity rec
  print a

  putStrLn "A wild stage appeared!"
  clutterMain

