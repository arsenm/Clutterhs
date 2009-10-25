module Main where

import Graphics.UI.Clutter hiding (show)
import qualified Graphics.UI.Clutter as C
import System.Glib.Attributes
import System.Glib.Signals (on, after)
import System.Mem
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)

import Random

cetiAlpha5 :: Alpha -> IO Double
cetiAlpha5 a = do
  tml <- alphaGetTimeline a
  prog <- timelineGetProgress tml
  rnd <- randomIO   --this results in hilarity.
  return (rnd * 5 * prog)


kirkAlpha :: Alpha -> IO Double
kirkAlpha a = do
  tml <- alphaGetTimeline a
  prog <- timelineGetProgress tml
  return (10 * prog)

main = do
  a <- clutterInit
  print a

  let c = Color 255 0 0 210
  let c2 = Color 0 0 123 123
  let c3 = Color 0 123 0 255
  let blue = Color 0 0 255 200
  let cnamed = fromMaybe (error "color parsing failed") (colorFromString "green")
  print cnamed

  rec <- rectangleNewWithColor c
  rec2 <- rectangleNewWithColor blue
  stg <- stageNew
  txt <- textNewWithText "sans" "CLUTTER HASKELL LOL"

  performGC

  tml <- timelineNew 9000
  alpha <- alphaNewFull tml Linear

 --FIXME: almost always end up needing to specify type for this
  anim <- animate rec Linear 3000 ("x", 400::Float)
                                  ("y", 500::Float)

  animtml <- animationGetTimeline anim
  timelineStart animtml

  on stg hide (clutterMainQuit)
  set stg [actorWidth := 800,
           actorHeight := 800]

  on stg buttonPressEvent $
       tryEvent $ do
         t <- eventTime
         coords <- eventCoordinates
         button <- eventButton
         flags <- eventFlags
         mod <- eventState
         dt <- eventDeviceType
         liftIO $ do
           putStrLn ("Flags: " ++ show flags)
           putStrLn ("Time: " ++ show t)
           putStrLn ("Coords: " ++ show coords)
           putStrLn ("Button: " ++ show button)
           putStrLn ("Modifier: " ++ show mod)
           putStrLn ("Device Type: " ++ show dt)


  containerAddActor stg rec
  containerAddActor stg rec2
  containerAddActor stg txt

  actorSetZRotationFromGravity rec2 45 GravityCenter

  actorSetPosition rec 50 50
  actorSetSize rec 100 100

  actorSetPosition rec2 200 350
  actorSetSize rec2 100 100

  (xpos, ypos) <- actorGetPosition rec
  print xpos
  print ypos
  stageSetColor stg cnamed

  actorSetPosition txt 200 300
  actorSetSize txt 100 30
  actorSetZRotationFromGravity txt 30 GravityCenter

  orig <- stageGetPerspective stg
  putStrLn $ "got orig: " ++ show orig

  let pers = Perspective 90 2 0.1 100

  putStrLn $ "setting to: " ++ show pers

  stageSetPerspective stg pers
  performGC

--either of these work
  onShow stg (putStrLn "I'm shown!")
--  on stg C.show (putStrLn "I'm shown!")

  out <- stageGetPerspective stg
  putStrLn $ "got out: " ++ show out

  actorShowAll stg

  actorGetZRotationGravity rec >>= print
  performGC

  putStrLn "A wild stage appeared!"

  --Try out alphas
  khan <- textNewWithText "sans bold 20" "KHAAAAAAAAAAAAAAN!!!!"
  containerAddActor stg khan
  actorSetPosition khan 10 100
  actorShow khan

  tl <- timelineNew 9001
  alphaParticle <- alphaNewWithFunc tl cetiAlpha5
  performGC

  --wtf is this needing :: IO (type)
  khanAnim <- (animateWithAlpha khan alphaParticle ("x", 300::Float) ("y", 300::Float)) :: IO Animation
  timelineStart tl

  performGC

  -- try behaviours
  behav <- behaviourRotateNew alphaParticle XAxis RotateCw 0 360
  behaviourRotateSetCenter behav 0 50 15
  behaviourApply behav rec2

  texture <- textureNewFromFile "khaaaaaaan.jpg"
  set texture [textureKeepAspectRatio := True]
  containerAddActor stg texture
  actorSetPosition texture 200 200

  actorSetSize texture 200 135
  actorShow texture

  performGC

  ka <- alphaNewWithFunc tl kirkAlpha

  --CHECKME: Doesn't work with RotateCcw but does with RotateCw??
  kirkBehav1 <- behaviourRotateNew ka ZAxis RotateCw 0 360
  behaviourRotateSetCenter kirkBehav1 100 100 100
  kirkBehav2 <- behaviourScaleNew ka 0.1 0.1 1 1

  performGC

  behaviourApply kirkBehav1 texture
  behaviourApply kirkBehav2 texture

  performGC

  clutterMain

