
import Prelude
import qualified Prelude as P
import Data.Maybe

import Graphics.UI.Clutter
import System.Glib.Signals
import System.Glib.Attributes


main = do
  clutterInit
  stage <- stageGetDefault

  text <- textNewFull "Webdings 12" "Comic Sans is the best font" (Color 255 0 0 255)
  set text [ actorPosition := (100, 200) ]

  let black = fromJust (colorFromString "black")
  pwText <- textNewFull "Sans 10" "This is a secret password" black
--  set pwText [ textPasswordChar := '*' ]
  --FIXME: Take  a charactor
  set pwText [ textPasswordChar := 12 ]
  set pwText [ actorPosition := (50, 300) ]

  containerAddActor stage text
  containerAddActor stage pwText

  actorShowAll stage

  clutterMain


