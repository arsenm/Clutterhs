
import System.Glib.Attributes
import Graphics.UI.Clutter hiding (show)
import System.Random

main = do
  clutterInit
  stg <- stageGetDefault
  txt <- textNewWithText "sans 30" "Translation"

  set txt [ actorWidth := 100,
            actorHeight := 30,
            actorX := 10,
            actorY := 200
          ]

  stg `containerAddActor` txt
  animate txt Linear 2000 [ actorX :-> 400 ]

  actorShowAll stg
  clutterMain

