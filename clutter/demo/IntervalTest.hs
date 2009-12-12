
import Graphics.UI.Clutter
import System.Glib.Attributes
import System.Glib.Signals
import Data.Maybe

import qualified System.Glib.GTypeConstants as GType


--customProgressFunc :: Float -> Float -> Double -> IO (Bool, Float)
customProgressFunc :: ProgressFunc Float
customProgressFunc a b p = let pf = realToFrac p
                           in return (True, (b - a) * sin (pf * 2 * pi))

main = do
  clutterInit
  stage <- stageGetDefault

  rec <- rectangleNewWithColor (Color 255 0 0 230)
  containerAddActor stage rec
  set rec [ actorX := 50,
            actorY := 50,
            actorWidth := 50,
            actorHeight := 50 ]

  ival1 <- intervalNew (50::Float) (500::Float)
  ival2 <- intervalNew (50::Float) (300::Float)

  anim <- animationNew :: IO (Animation Rectangle)
  animationSetObject anim rec

  animationSetDuration anim 2500
  animationBindInterval anim actorX  ival1
  animationBindInterval anim actorY  ival2


  intervalRegisterProgressFunc GType.float customProgressFunc

  tml <- fmap fromJust (animationGetTimeline anim)
  timelineStart tml


  on stage hide mainQuit
  actorShowAll stage
  clutterMain

