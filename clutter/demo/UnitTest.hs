
import Graphics.UI.Clutter
import Data.Maybe


main :: IO ()
main = do
  clutterInit
  unitPx <- unitsFromPixels 9001
  unitStr <- fmap fromJust (unitsFromString "5.1 em")
  unitMm <- unitsFromMm 42

  unitsToPixels unitStr >>= print

  unitsToPixels unitMm >>= print

  unitsToString unitPx >>= print
  unitsToString unitStr >>= print