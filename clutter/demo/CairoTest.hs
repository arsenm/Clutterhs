
import Graphics.Rendering.Cairo
import Graphics.UI.Clutter hiding (paint)
import System.Glib.Attributes
import System.Glib.Signals
import Control.Applicative
import Control.Arrow

--actorGetSize a = liftA2 (,) (actorGetWidth a) (actorGetHeight a)
conv = (realToFrac *** realToFrac)


drawHappyFace :: CairoTexture -> Render ()
drawHappyFace crtxt = do
  (w,h) <- liftIO (conv <$> actorGetSize crtxt)

  let r = w / 2 - 20
  setOperator OperatorSource
  setSourceRGBA 0.4 0.9 0.3 0.8
  setLineWidth 10
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  arc (w/2) (h/2) r 0 (2*pi)
  stroke

  arc (0.4 * w) (h/3) (r/12) 0 (2*pi)
  stroke

  arc (0.6 * w) (h/3) (r/12) 0 (2*pi)
  stroke

  arc (w/2) (0.6 * h) (r/3) 0 pi
  stroke


main :: IO ()
main = do
  clutterInit
  stg <- stageGetDefault

  set stg [ actorWidth := 800,
            actorHeight := 800 ]

  crtxt <- cairoTextureNew 150 150
  containerAddActor stg crtxt
  actorSetAnchorPointFromGravity stg GravityCenter
  actorSetAnchorPointFromGravity crtxt GravityCenter
  set crtxt [ actorRotationCenterZGravity := GravityCenter,
              actorRotationAngleZ := 45,
              actorX := 400,
              actorY := 400 ]

  renderWithCairoTexture crtxt (drawHappyFace crtxt)

  animate crtxt Linear 10000 [ actorRotationAngleZ :-> 2000, actorWidth :-> 700, actorHeight :-> 700 ]

  actorShowAll stg
  clutterMain


