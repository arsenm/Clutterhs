
import Graphics.UI.Clutter
import System.Glib.Attributes
import System.Glib.Signals
import Data.Maybe


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

--FIXME: newAnimation and Co. exposed and huge mistakes happen
--FIXME: Wrap animation type to tell what is animated
  anim <- animationNew
  animationSetObject anim rec
  animationSetDuration anim 2500
  animationBindInterval anim (actorX::Attr Rectangle Float)  ival1
  animationBindInterval anim (actorY::Attr Rectangle Float)  ival2

  tml <- fmap fromJust (animationGetTimeline anim)
  timelineStart tml


  on stage hide mainQuit
  actorShowAll stage
  clutterMain

