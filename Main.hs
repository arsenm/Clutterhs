
module Main where

import Graphics.UI.Clutter
import System.Mem

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  a <- clutterInit
  print a
  putStrLn "Wow...omg"

  c <- colorNew 123 0 0 123
  rec <- rectangleNewWithColor c
  stg <- stageNew

  containerAddActor stg rec

  actorSetPosition rec 50 50
  actorSetSize rec 100 100
  (xpos, ypos) <- actorGetPosition rec
  print xpos
  print ypos
--  print c
--  d <- colorRed c
--  print d
--  putStrLn "Yay!"
--  stageSetColor stg c

  orig <- stageGetPerspective stg
  putStrLn $ "got orig: " ++ show orig


  let pers = Perspective 90 2 0.1 100

  putStrLn $ "setting to: " ++ show pers

  stageSetPerspective stg pers
  performGC

  actorShow rec
  actorShow stg

  out <- stageGetPerspective stg
  putStrLn $ "got out: " ++ show out

  actorShowAll stg

  a <- actorGetZRotationGravity rec
  print a

--  withPerspective pers


  putStrLn "A wild stage appeared!"
  clutterMain

