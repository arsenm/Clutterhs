
import Prelude
import qualified Prelude as P
import Data.Maybe

import Graphics.UI.Clutter
import System.Glib.Signals
import System.Glib.Attributes


main = do
  clutterInit
  stage <- stageGetDefault

  text <- textNewFull "Comic Sans MS 12" "Comic Sans is the best font" (Color 255 0 0 255)
  set text [ actorX := 100,
             actorY := 200 ]

  text2 <- textNewFull "Comic Sans MS 25" "TIM LOVES COMIC SANS" (Color 123 3 123 255)
  set text2 [ actorX := 220,
              actorY :=  10,
              actorRotationAngleZ := 45]


  let black = fromJust (colorFromString "black")
  pwText <- textNewFull "Sans 10" "This is a secret password" black
--  set pwText [ textPasswordChar := '*' ]
  --FIXME: Take  a charactor
  set pwText [ textPasswordChar := Just 0x2665,
               actorX := 50,
               actorY := 300 ]

  textEdit <- textNewFull "San 12" "Edit me" black
  set textEdit [ actorX := 400,
                 actorY := 350,
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


  mapM_ (containerAddActor stage) [text, text2, pwText, textEdit]

  actorShowAll stage

  clutterMain


