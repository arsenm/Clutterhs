--A simple test of everyone's favorite actor

import Prelude
import qualified Prelude as P
import Graphics.UI.Clutter
import System.Glib.Signals
import System.Glib.Attributes
import Control.Monad
import Control.Monad.Trans (liftIO)
import System.IO
import Data.Maybe


main = do
  clutterInit
  stg <- stageGetDefault

  rec1 <- rectangleNew
  rec2 <- rectangleNewWithColor (Color 123 211 232 233)
  rec3 <- rectangleNew

  rectangleSetColor rec1 (Color 255 123 0 200)

  rectangleSetColor rec2 (fromJust $ colorFromString "green")
  rectangleSetColor rec3 (fromJust $ colorFromString "turquoise")

  bc1 <- rectangleGetBorderColor rec1
  putStrLn $ "Rec1 border: " ++ P.show bc1

  rectangleSetBorderColor rec3 (fromJust $ colorFromString "black")
  rectangleSetBorderWidth rec3 10

  mapM_ (containerAddActor stg) [rec1,rec2,rec3]
  mapM (\a -> actorSetSize a 50 50) [rec1, rec2, rec3]

  set rec1 [actorPosition := (100, 100)]
  set rec2 [actorPosition := (200, 200)]
  set rec3 [actorPosition := (300, 300)]

--Damage needs to happen to see the border change
--Also only visible on one, since others default width to 0
  mapM_ (\r -> actorSetReactive r True >>
               (on r buttonPressEvent $
                 tryEvent $ liftIO $ do
                   set r [ rectangleHasBorder :~ not ])) [rec1, rec2, rec3]

  actorShowAll stg

  clutterMain

