{-# OPTIONS_GHC -Wall #-}

import Prelude
import qualified Prelude as P

import Graphics.UI.Clutter

import qualified Graphics.UI.Clutter.GTypes as CGT
import qualified System.Glib.GTypeConstants as GT
import System.Glib.GObject



main = do
  clutterInit

  tml <- timelineNew 3000
  alp <- alphaNewFull tml Linear
  bo <- behaviourOpacityNew (Just alp) 255 10

  let a = isA bo CGT.behaviour
  let b = isA bo CGT.behaviouropacity
  let c = isA bo CGT.behaviourellipse
  let d = isA bo GT.object
  print [a,b,c,d]
  putStrLn "arst"
  ival1 <- intervalNew (4::Int) 500

  putStrLn "Oh wow"
  intervalGetInitialValue ival1 >>= \s -> putStrLn $ "Initial value: " ++ P.show s
  intervalGetFinalValue ival1 >>= \s -> putStrLn $ "Final value: " ++ P.show s
  intervalComputeValue ival1 0.5 >>= \s -> putStrLn $ "Computed: " ++ P.show s
  putStrLn "Hello"

  clutterMain

