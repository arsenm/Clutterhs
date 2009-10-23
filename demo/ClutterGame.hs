module Main where

import Graphics.UI.Clutter hiding (Nothing)
import System.Glib.Attributes
import System.Glib.Signals
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.Word (Word8)

import Random (randomIO)

colors = map (fromJust . colorFromString) ["blanched almond",
	                                   "OldLace",
	                                   "MistyRose",
	                                   "White",
	                                   "LavenderBlush",
	                                   "CornflowerBlue",
	                                   "chartreuse",
	                                   "chocolate",
	                                   "light coral",
	                                   "medium violet red",
	                                   "LemonChiffon2",
	                                   "RosyBrown3"];
white = Color 255 255 255 255

createRectangles::Stage ->
                  Float ->
                  (Maybe Animation, [Rectangle], Float) ->
                  Color ->
                  IO (Maybe Animation, [Rectangle], Float)
createRectangles stage size (_, recs, i) col = do
  rec <- rectangleNewWithColor col
  containerAddActor stage rec
  set rec [ actorHeight := size,
            actorWidth := size,
            actorAnchorPointGravity := GravityCenter,
            actorY := i*size + size/2 ]

  width <- get stage actorWidth
  anim <- animate rec Linear 5000 ("x", width/2) ("rotation-angle-z", 500::Double)
  return (Just anim, rec:recs, i+1)

scatterRectangle :: Float -> Float -> Rectangle -> IO ()
scatterRectangle h w rec = do
  rnd1 <- randomIO
  rnd2 <- randomIO
  --rotation_angle change makes it stop spinning. don't know why it's still
  --spinning here, it really should have stopped when the timeline did.
  (ang,_,_,_) <- actorGetRotation rec ZAxis
  animate rec EaseOutBounce 3000 ("x", rnd1 * w)
                                 ("y", rnd2 * h / 2 + h / 2)
                                 ("rotation-angle-z", ang)
                                 ("opacity", 0::Word8)

--FIXME: Why did this need a type signature?
completedAnimation :: Stage -> IO Animation
completedAnimation stage = do
  text <- textNewWithText "Bitstream Vera Sans 40" "Congratulations!"
  w <- get stage actorWidth
  h <- get text actorHeight
  stageheight <- get stage actorHeight
  set text [textColor := white,
            actorAnchorPointGravity := GravityCenter,
            actorX := w / 2,
            actorY := (-h)  -- off stage
           ]
  containerAddActor stage text
  animate text EaseOutBounce 3000 ("y", stageheight / 2)

main = do
  clutterInit

  stage <- stageGetDefault
  set stage [stageColor := Color 0 0 0 255]

  on stage hide clutterMainQuit

  height <- get stage actorHeight
  width <- get stage actorWidth
  let size = height / fromIntegral (length colors)

  --TODO: Can do animations with signals in name
  --FIXME: This takes...2 arguments....function in your gvalue...triple...this...requires making work.
  (Just finAnim, recs, _) <- foldM (createRectangles stage size) (Nothing, [], 0) colors
  tml <- get finAnim animationTimeline

  on tml completed (completedAnimation stage >> mapM_ (scatterRectangle height width) recs)

  actorShowAll stage
  clutterMain

