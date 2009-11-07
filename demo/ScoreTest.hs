
import Prelude
import qualified Prelude as P

import Graphics.UI.Clutter
import System.Glib.Signals
import System.Glib.Attributes


main :: IO ()
main = do
  clutterInit

  score <- scoreNew

  tml1 <- timelineNew 1000
  tml2 <- timelineNew 2000
  tml3 <- timelineNew 3000
  tml4 <- timelineNew 4000
  tml5 <- timelineNew 5000

  timelineAddMarkerAtTime tml1 "Halfway through first timeline" 500
  timelineAddMarkerAtTime tml2 "arst" 1000
  timelineAddMarkerAtTime tml2 "Somewhere in the third timeline" 1600
  timelineAddMarkerAtTime tml4 "Start here" 2000

  scoreAppend score P.Nothing tml1
  scoreAppend score (Just tml1) tml2
  scoreAppend score (Just tml1) tml3
  scoreAppend score (Just tml2) tml4
  scoreAppendAtMarker score tml4 "Start here" tml5

  on score timelineCompleted $ \tmlDone -> do dur <- timelineGetDuration tmlDone
                                              putStrLn ("Timeline of duration " ++ P.show dur ++ " completed")

  on score timelineStarted $ \tmlStart -> do dur <- timelineGetDuration tmlStart
                                             putStrLn ("Timeline of duration " ++ P.show dur ++ " started")

  scoreStart score

  tmlList <- scoreListTimelines score

  mapM_ (\tml -> do d <- timelineGetDuration tml
                    putStrLn ("Listing timeline of duration: " ++ P.show d)) tmlList

--on score completed $ putStrLn "Score completed" >> clutterMainQuit

  clutterMain

