
module Main where

import Graphics.UI.Clutter

import Data.List

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  putStrLn "Wow...omg"
  col <- colorNew 0 0 0 0
  putStrLn "Yay!"

