
module Main where

import Graphics.UI.Clutter
import Foreign.Ptr
import Data.List

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  putStrLn "Wow...omg"
  c <- colorNew 0 0 0 0
  print c
  putStrLn "Yay!"

