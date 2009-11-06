
import Prelude
import qualified Prelude as P
import Data.Maybe

import Graphics.UI.Clutter
import System.Glib.Signals
import System.Glib.Attributes


main = do
  clutterInit
  stage <- stageGetDefault

  actorSetSize stage 800 800

  tml <- timelineNew 3000
  set tml [ timelineLoop := True ]
  alp <- alphaNewFull tml Linear

  rec <- rectangleNewWithColor (Color 255 100 0 225)
  rec2 <- rectangleNewWithColor (Color 0 100 255 225)
  rec3 <- rectangleNewWithColor (Color 0 255 100 225)
  rec4 <- rectangleNewWithColor (Color 123 123 123 225)
  rec5 <- rectangleNewWithColor (Color 255 0 255 225)

  mapM_ (flip set [actorSize := (50,50)] ) [rec, rec2, rec3, rec4, rec5]
  set rec [actorPosition := (300, 300)]
  set rec2 [actorPosition := (400, 300)]
  set rec3 [actorPosition := (500, 300)]

  --TODO: What does Nothing for an alpha mean? Maybe for Alphas is annoying
  --I'm not sure that this is a useful feature in Clutter
  -- ask someone if there's a reason for it, since creating a behaviour
  --for sitting there is stupid
  -- also the position doesn't seem to work if the alpha is nothing
  behav <- behaviourEllipseNew (Just alp) 400 400 400 300 RotateCw 0 0
  behav2 <- behaviourEllipseNew (Just alp) 200 100 400 100 RotateCcw 45 270
  behav3 <- behaviourEllipseNew  (Just alp) 300 300 100 100  RotateCw 0 0
  behav4 <- behaviourEllipseNew  P.Nothing 300 300 100 100  RotateCw 0 0
  behav5 <- behaviourEllipseNew (Just alp) 400 400 400 300 RotateCw 0 0

  behaviourEllipseSetAngleTilt behav5 ZAxis 45

  mapM_ (containerAddActor stage) [rec, rec2, rec3, rec4, rec5]

  behav `behaviourApply` rec
  behav2 `behaviourApply` rec2
  behav3 `behaviourApply` rec3
  behav4 `behaviourApply` rec4
  behav5 `behaviourApply` rec5

  actorShowAll stage

  timelineStart tml

  clutterMain

