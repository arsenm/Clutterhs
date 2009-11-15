
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
  set pwText [ textPasswordChar := 0x2665,
               actorPosition := (50, 300) ]

  textEdit <- textNewFull "San 12" "Edit me" black
  set textEdit [ actorPosition := (400, 350),
                 actorReactive := True,
               --textCursorColor := P.Nothing,
                 textCursorColor := Just (Color 123 210 231 230),
                 textCursorSize := 3,
                 textEditable := True ]
  putStr "Cursor color: "
  get textEdit textCursorColor >>= print

  coords <- textPositionToCoords textEdit 4
  putStrLn ("textPositionToCoords: " ++ P.show coords)

  on textEdit activate (putStrLn "I'm activated")
  on textEdit cursorEvent (\geom -> putStrLn $ "Geom: " ++ P.show geom)
  on textEdit textChanged (get textEdit textText >>= putStrLn)


  containerAddActor stage text
  containerAddActor stage pwText
  containerAddActor stage textEdit

  actorShowAll stage

  clutterMain


