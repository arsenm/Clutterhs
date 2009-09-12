
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

  c <- colorNew 123 0 0 0
  rec <- rectangleNewWithColor c
  stg <- stageNew
  actorSetParent rec stg
  actorSetPosition rec 100 100
  (xpos, ypos) <- actorGetPosition rec
  print xpos
  print ypos
--  print c
--  d <- colorRed c
--  print d
--  putStrLn "Yay!"

  actorShow stg
  actorShow rec

--  performGC
  putStrLn "A wild stage appeared!"
  clutterMain

