
-- Transparent stage

import Graphics.UI.Clutter
import Graphics.UI.Clutter.X11
import System.Glib.Signals
import System.Glib.Attributes

import Control.Monad.Trans (liftIO)

main = do
  x11SetUseARGBVisual True
  clutterInit

  stg <- stageGetDefault
  stageSetUseAlpha stg True
  stageSetColor stg (Color 100 100 100 100)

  r1 <- rectangleNewWithColor (Color 200 100 100 100)
  r2 <- rectangleNewWithColor (Color 100 100 200 100)
  set r1 [ actorWidth := 100, actorHeight := 100,
           actorX := 200, actorY := 200
         ]
  set r2 [ actorWidth := 100, actorHeight := 100,
           actorX := 250, actorY := 250
         ]

  stg `on` buttonPressEvent $ liftIO (putStrLn "Clicked" >> return True)

  containerAddActor stg r1
  containerAddActor stg r2
  stg `on` hide $ mainQuit
  actorShowAll stg
  clutterMain

