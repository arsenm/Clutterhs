
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
                  Float ->
                  (Maybe Animation, [Timeline], Float) ->
                  Color ->
                  IO (Maybe Animation, [Timeline], Float)
createRectangles stage w h (_, fts, i) col = do
  let size = h / fromIntegral (length colors)
  rec <- rectangleNewWithColor col
  containerAddActor stage rec
  set rec [ actorHeight := size,
            actorWidth := size,
            actorAnchorPointGravity := GravityCenter,
            actorY := i*size + size/2 ]

  width <- get stage actorWidth
  anim <- animate rec Linear 5000 ("x", width/2) ("rotation-angle-z", 500::Double)
  tml <- get anim animationTimeline
  timelineStart tml

  --TODO:Signal connect the final thing...but that would be on each, not on final one
  rnd1 <- randomIO
  rnd2 <- randomIO
  finAnim <- animate rec EaseOutBounce 3000 ("x", rnd1 * w)
                                            ("y", rnd2 * h / 2 + h / 2)
                                          --("rotation-angle-z",) to what it was? what?
                                          --("opacity", 0::Word8)
  finTml <- get finAnim animationTimeline
  return (Just anim, finTml:fts, i+1)


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
  actorShow text
  anim <- animate text EaseOutBounce 3000 ("y", stageheight / 2)
  get anim animationTimeline >>= timelineStart

main = do
  clutterInit

  stage <- stageNew
  set stage [stageColor := Color 0 0 0 255]

  on stage hide clutterMainQuit


  height <- get stage actorHeight
  width <- get stage actorWidth

  --TODO: Can do animations with signals in name

  (Just finAnim, finTmls, _) <- foldM (createRectangles stage width height) (Nothing, [], 0) colors
  tml <- get finAnim animationTimeline


  on tml completed (mapM_ timelineStart finTmls)

  actorShowAll stage
  clutterMain

