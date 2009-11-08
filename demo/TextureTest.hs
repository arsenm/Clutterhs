
import Prelude hiding (catch)
import qualified Prelude as P

import Graphics.UI.Clutter
import System.Glib.GError


main = do
  clutterInit
  stage <- stageGetDefault

  rec1 <- rectangleNewWithColor (Color 255 0 0 0)

  actorSetPosition rec1 200 300
  actorSetSize rec1 50 50

  txtrFail <- textureNewFromActor rec1
  case txtrFail of
    P.Nothing -> putStrLn "textureNewFromActor failed"
    _ -> putStrLn "new from actor worked"

  putStrLn "Trying to get nonexistant file"
--  texture <- textureNewFromFile "ThisFileDoesNotExist"

  failedTexture <- catchGError (textureNewFromFile "ThisFileDoesNotExist") (\_ -> putStrLn "Handle" >> return P.Nothing)


  actorShowAll stage
  clutterMain


