
import Prelude hiding (catch)
import qualified Prelude as P
import Data.Maybe

import Graphics.UI.Clutter
import System.Glib.GError
import System.Glib.Attributes
import System.Glib.Signals

import Data.Array.MArray


main = do
  clutterInit
  stage <- stageGetDefault
  actorSetSize stage 800 800

  rec1 <- rectangleNewWithColor (Color 255 0 0 0)
  rec2 <- rectangleNewWithColor (Color 0 255 0 0)

  actorSetPosition rec1 200 300
  actorSetSize rec1 50 50

  actorSetPosition rec2 150 400
  actorSetSize rec1 100 50

  txtrFail <- textureNewFromActor rec1
  case txtrFail of
    P.Nothing -> putStrLn "textureNewFromActor failed"
    _ -> putStrLn "new from actor worked"

  putStrLn "Trying to get nonexistent file"
--  texture <- textureNewFromFile "ThisFileDoesNotExist"

  failedTexture2 <- catchGError (fmap Just $ textureNewFromFile "ThisFileDoesNotExist")
                                (\_ -> putStrLn "It did indeed fail" >> return P.Nothing)

--CHECKME: Exception in handler?
  emptyTexture <- textureNew
  onLoadFinished emptyTexture (\err -> case err of
                                         P.Nothing -> putStrLn "Load ok!"
                                         Just (GError a b c) -> putStrLn $ "Load finished Failure: "
                                                                  ++ P.show a ++ " "
                                                                  ++ P.show b ++ " " ++ P.show c)

--  textureSetFromFile emptyTexture "ThisFileDoesn'tExist"
  textureSetFromFile emptyTexture "khaaaaaaan.jpg"

  squirrelfish <- textureNewFromFile "SquirrelFish.svg"
  containerAddActor stage squirrelfish
  set squirrelfish [ actorX := 100,
                     actorY := 100,
                     textureKeepAspectRatio := True,
                     textureFilterQuality := TextureQualityHigh,
                     actorWidth := 300 ]

{-
I'm not sure if I'm using it wrong, or if the binding is wrong,
but this ends in assertion failures
  actorShow squirrelfish
  sfCopy <- fmap fromJust (textureNewFromActor squirrelfish)
  containerAddActor stage sfCopy

  set sfCopy [ actorWidth := 50,
               actorPosition := (400, 400),
               textureRepeat := (True, True) ]
-}


  sfClone <- cloneNew (Just squirrelfish)
  set sfClone [ actorWidth := 50,
                actorX := 400,
                actorY := 400 ]

  putStrLn "Attempting to read pixels"
  pixels <- fmap (fromMaybe (P.error "stageReadPixels failed"))
             --  (stageReadPixels stage 150 150 300 300)
             --  (stageReadPixels stage 100 100 799 799)
                 (stageReadPixels stage 100 100 (-1) (-1))
             --  (stageReadPixels stage 130 380 200 200)

--  putStrLn $ "READARRAY: " ++ P.show (readArray pixels (5,10))

  pixTx <- textureNew
  putStrLn "Attempting to set from rgb data"
  success <- catchGError (textureSetFromRgbData pixTx pixels [TextureNone])
                         (\(GError a b c) -> P.error $ P.show a ++ " " ++ P.show b ++ " " ++ P.show c)
  putStrLn $ "Set from rgb sucess: " ++ P.show success

  containerAddActor stage pixTx
  set pixTx [ actorWidth := 100,
              actorHeight := 100,
              actorX := 500,
              actorY := 500 ]


  containerAddActor stage sfClone
  actorShowAll stage
  clutterMain


