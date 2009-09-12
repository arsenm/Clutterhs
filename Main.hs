
module Main where

import Graphics.UI.Clutter
import Foreign.Ptr
import Data.List

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  a <- clutterInit
  print a
  putStrLn "Wow...omg"

--  c <- colorNew 1 2 3 4
--  print c
--  d <- colorRed c
--  print d
--  putStrLn "Yay!"
  stg <- stageNew
  actorShow stg
  putStrLn "Hello stage? I hope?"

