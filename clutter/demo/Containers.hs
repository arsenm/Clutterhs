
import Graphics.UI.Clutter hiding (show)
import System.Glib.Attributes
import System.Glib.Signals
import Control.Monad.Trans (liftIO)

main = do
  clutterInit
  stg <- stageGetDefault

  rec <- rectangleNewWithColor (Color 180 220 150 185)
  actorSetPosition rec 200 200
  actorSetSize rec 100 100
  actorSetReactive rec True

  rec `on` buttonPressEvent $ liftIO (actorDestroy rec >> return True)

  stg `on` actorAdded   $ \_ -> putStrLn "I consume children"
  stg `on` actorRemoved $ \_ -> putStrLn "Nooo they be stealin my children"

  containerAddActor stg rec

  actorShowAll stg

  clutterMain

