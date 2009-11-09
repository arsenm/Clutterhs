
import Prelude
import qualified Prelude as P

import Graphics.UI.Clutter
import System.Glib.Signals
import System.Glib.Attributes


main = do
  clutterInit
  stage <- stageGetDefault

  text <- textNewFull "Webdings 12" "Comic Sans is the best font" (Color 255 0 0 255)
  set text [ actorPosition := (100, 200) ]

  containerAddActor stage text

  actorShowAll stage

  clutterMain


