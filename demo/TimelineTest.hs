
import Prelude
import qualified Prelude as P
import Graphics.UI.Clutter
import System.Glib.Signals
import Control.Monad
import System.IO


main = do
  clutterInit
  tml <- timelineNew 9001
  tmlClone <- timelineClone tml

--test clone
  cloneDur <- timelineGetDuration tmlClone
  cloneDel <- timelineGetDelay tmlClone
  putStrLn ("Cloned timeline duration: " ++ P.show cloneDur)
  putStrLn ("Cloned timeline delay: " ++ P.show cloneDel)

  timelineSetLoop tml True

  isPlayClone <- timelineIsPlaying tmlClone
  putStrLn ("Clone playing: " ++ P.show isPlayClone)

  timelineAddMarkerAtTime tml "arst" 2000

  hasAoeu <- timelineHasMarker tml "aoeu"
  hasArst <- timelineHasMarker tml "arst"
  putStrLn ("Timeline has aoeu: " ++ P.show hasAoeu)
  putStrLn ("Timeline has arst: " ++ P.show hasArst)

  timelineAddMarkerAtTime tml "asdf" 7000
  timelineAddMarkerAtTime tml "qsdf" 1000
  timelineAddMarkerAtTime tml "removeme" 3000

  on tml markerReached (\name ms -> putStrLn $ "Marker reached: " ++
                                     P.show name ++ ": " ++ P.show ms ++ "ms")

  on tml completed clutterMainQuit

  on tml started (putStrLn "Timeline started")


  timelineAdvance tml 500

  elap <- timelineGetElapsedTime tml
  prog <- timelineGetProgress tml
  putStrLn $ "Timeline elapsed: " ++ P.show elap ++ " progress: " ++ P.show prog

  timelineAdvanceToMarker tml "arst"

  clonePlay2 <- timelineIsPlaying tmlClone
  putStrLn $ "Clone play 2: " ++ P.show clonePlay2

  --CHECKME: Is this freed
  markers <- timelineListMarkers tml (-1)
  putStrLn "Markers before removal:"
  mapM_ print markers

  timelineRemoveMarker tml "removeme"

  markersAfter <- timelineListMarkers tml (-1)
  putStrLn "Markers after removal:"
  mapM_ print markersAfter


  timelineStart tml

  clutterMain

