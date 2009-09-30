-- -*-haskell-*-
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Types (
--                                  withGObject,
--                                  withGObjectClass,

                                  Color(Color),
                                  ColorPtr,
                                  withColor,

                                  Actor,
                                  ActorClass,
                                  withActor,
                                  withActorClass,
                                  toActor,

                                  Rectangle,
                                  RectangleClass,
                                  toRectangle,
                                  withRectangle,
                                  newRectangle,
--                                  constructRectangle,

                                  Text,
                                  TextClass,
                                  toText,
                                  withText,
                                  newText,

                                  Stage,
                                  StageClass,
                                  withStage,
                                  newStage,
                               --   constructStage,

                                  Container,
                                  ContainerClass,
                                  toContainer,
                                  unContainer,
                                  withContainer,
                                  withContainerClass,

                                  Perspective(Perspective),
                                  PerspectivePtr,
                                  withPerspective,
                                  PickMode(..),
                                  Gravity(..),
                                  RequestMode(..),
                                  ActorFlags(..),
                                  AllocationFlags(..),
                                  RotateAxis(..),

                                  InitError(..),

                                  Event,

                                  EventType(..),
                                  EventFlags(..),
                                  ModifierType(..),
                                  StageState(..),
                                  ScrollDirection(..),

                                  Animation,
                                  AnimationClass,
                                  toAnimation,
                                  withAnimation,
                                  newAnimation,

                                  Timeline,
                                  TimelineClass,
                                  withTimeline,
                                  newTimeline,

                                  Alpha,
                                  AlphaClass,
                                  withAlpha,
                                  newAlpha,

                                  AnimationMode(..),

                                  TimelineDirection(..),

                                  Interval,
                                  IntervalClass,
                                  withInterval,
                                  newInterval

                                 ) where

--FIXME: Conflict with EventType Nothing
import Prelude hiding (Nothing)

import C2HS
import System.Glib.GObject
import System.Glib.Flags
import Foreign.ForeignPtr
import Control.Monad (liftM)
import Control.Exception (bracket)

--this doesn't seem to work since GObjectClass is not here...
--I'm not sure if I can work around this. Oh well, I don't think it's that important
--{# pointer *GObject newtype nocode #}
--{# class GObjectClass GObject #}

-- g-types not anywhere??
type GUInt8 = {# type guint8 #}
type GFloat = {# type gfloat #}


-- *************************************************************** Misc

{# enum ClutterInitError as InitError {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterPickMode as PickMode {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterAllocationFlags as AllocationFlags {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterGravity as Gravity {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterActorFlags as ActorFlags {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterRequestMode as RequestMode {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterRotateAxis as RotateAxis {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterEventType as EventType {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterEventFlags as EventFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}
{# enum ClutterModifierType as ModifierType {underscoreToCase} deriving (Show, Eq, Bounded) #}
{# enum ClutterStageState as StageState {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterScrollDirection as ScrollDirection {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterTimelineDirection as TimelineDirection {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterAnimationMode as AnimationMode {underscoreToCase} deriving (Show, Eq) #}

--FIXME/TODO: ModifierType one at least fails everytime I try to use
--it because toEnum can't match 3...but why is it trying? silly bits.
--also using Bounded, I think goes through all 32 bits
--but it uses 1..12, 26,27,28, and 30, and a crazy mask.
--Figure it out later.
instance Flags ModifierType
instance Flags EventFlags

--maybe a not right hackishness?
--withGObjectClass::GObjectClass o => o -> (Ptr GObject -> IO b) -> IO b
--withGObjectClass o = (withGObject . toGObject) o
--This really should work. It was working. Something bad is happening.
--withGObject (GObject fptr) = withForeignPtr fptr

-- ***************************************************************

-- *************************************************************** Color

{-
{# pointer *ClutterColor as Color foreign newtype #}

instance Show Color where
  show (Color c) = show c

unColor (Color o) = o

manageColor :: Color -> IO ()
manageColor (Color colorForeignPtr) = do
  addForeignPtrFinalizer colorFree colorForeignPtr

foreign import ccall unsafe "&clutter_color_free"
  colorFree :: FinalizerPtr Color

mkColor :: Ptr Color -> IO Color
mkColor colorPtr = do
  colorForeignPtr <- newForeignPtr colorFree colorPtr
  return (Color colorForeignPtr)
-}

{# pointer *ClutterColor as ColorPtr -> Color #}

data Color = Color { red :: GUInt8,
                     green :: GUInt8,
                     blue :: GUInt8,
                     alpha :: GUInt8
                   } deriving (Eq, Show)

instance Storable Color where
  sizeOf _ = {# sizeof ClutterColor #}
  alignment _ = alignment (undefined :: GUInt8)
  peek p = do
      red <- {# get ClutterColor->red #} p
      blue <- {# get ClutterColor->blue #} p
      green <- {# get ClutterColor->green #} p
      alpha <- {# get ClutterColor->alpha #} p
      return $ Color (cIntConv red) (cIntConv green) (cIntConv blue) (cIntConv alpha)
      --FIXME: cIntConv and GUInt8 = ???

  poke p (Color r g b a) = do
      {# set ClutterColor->red #} p (cIntConv r)   --FIXME: cIntConv is wrong?
      {# set ClutterColor->green #} p (cIntConv g)
      {# set ClutterColor->blue #} p (cIntConv b)
      {# set ClutterColor->alpha #} p (cIntConv a)
      return ()

--This seems not right. But it seems to work.
mkColor :: Color -> IO ColorPtr
mkColor col = do cptr <- (malloc :: IO ColorPtr)
                 poke cptr col
                 return cptr

withColor :: Color -> (ColorPtr -> IO a) -> IO a
withColor col = bracket (mkColor col) free


-- *************************************************************** Actor

{# pointer *ClutterActor as Actor foreign newtype #}
--{# class GObjectClass => ActorClass Actor #}

class GObjectClass o => ActorClass o
toActor::ActorClass o => o -> Actor
toActor = unsafeCastGObject . toGObject
withActorClass::ActorClass o => o -> (Ptr Actor -> IO b) -> IO b
withActorClass o = (withActor . toActor) o

instance ActorClass Actor
instance GObjectClass Actor where
  toGObject (Actor a) = mkGObject (castForeignPtr a)
  unsafeCastGObject (GObject o) = Actor (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Rectangle

{# pointer *ClutterRectangle as Rectangle foreign newtype #}

class GObjectClass o => RectangleClass o
toRectangle::RectangleClass o => o -> Rectangle
toRectangle = unsafeCastGObject . toGObject

newRectangle :: Ptr Actor -> IO Rectangle
newRectangle a = makeNewGObject Rectangle $ return (castPtr a)
--constructRectangle a = constructNewGObject Rectangle $ return (castPtr a)

instance RectangleClass Rectangle
instance ActorClass Rectangle
instance GObjectClass Rectangle where
  toGObject (Rectangle r) = mkGObject (castForeignPtr r)
  unsafeCastGObject (GObject o) = Rectangle (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Text

{# pointer *ClutterText as Text foreign newtype #}

class GObjectClass o => TextClass o
toText::TextClass o => o -> Text
toText = unsafeCastGObject . toGObject

newText :: Ptr Actor -> IO Text
newText a = makeNewGObject Text $ return (castPtr a)

instance TextClass Text
instance ActorClass Text
instance GObjectClass Text where
  toGObject (Text a) = mkGObject (castForeignPtr a)
  unsafeCastGObject (GObject o) = Text (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Group

{#pointer *ClutterGroup as Group foreign newtype #}

class ActorClass o => GroupClass o
toGroup :: GroupClass o => o -> Group
toGroup = unsafeCastGObject . toGObject

instance GroupClass Group
instance ContainerClass Group
instance ActorClass Group
instance GObjectClass Group where
  toGObject (Group g) = mkGObject (castForeignPtr g)
  unsafeCastGObject (GObject o) = Group (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Container

{# pointer *ClutterContainer as Container foreign newtype #}

mkContainer = Container
unContainer (Container o) = o

class GObjectClass o => ContainerClass o
toContainer :: ContainerClass o => o -> Container
toContainer = unsafeCastGObject . toGObject

withContainerClass::ContainerClass o => o -> (Ptr Container -> IO b) -> IO b
withContainerClass o = (withContainer . toContainer) o

instance ContainerClass Container
instance GObjectClass Container where
  toGObject (Container c) = mkGObject (castForeignPtr c)
  unsafeCastGObject (GObject o) = Container (castForeignPtr o)

-- ***************************************************************


-- *************************************************************** Stage

{# pointer *ClutterStage as Stage foreign newtype #}

class GroupClass o => StageClass o
toStage :: StageClass o => o -> Stage
toStage = unsafeCastGObject . toGObject

--FIXME?? Is this OK, with casting? Not always true?
--FIXME: Name and convention for this type deal.
newStage:: Ptr Actor -> IO Stage
newStage a = makeNewGObject Stage $ return (castPtr a)
--constructStage:: Ptr Actor -> IO Stage
--constructStage a = constructNewGObject Stage $ return (castPtr a)

instance StageClass Stage
instance ContainerClass Stage
instance GroupClass Stage
instance ActorClass Stage
instance GObjectClass Stage where
  toGObject (Stage s) = mkGObject (castForeignPtr s)
  unsafeCastGObject (GObject o) = Stage (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Perspective

--FIXME: How to marshal this?
data Perspective = Perspective {
      perspectiveFovy :: !Float,
      perspectiveAspect :: !Float,
      perspectiveZNear :: !Float,
      perspectiveZFar :: !Float
    } deriving (Show, Eq)

{# pointer *ClutterPerspective as PerspectivePtr -> Perspective #}

instance Storable Perspective where
  sizeOf _ = {# sizeof ClutterPerspective #}
  alignment _ = alignment (undefined :: CFloat)
  peek p = do
      fovy <- {# get ClutterPerspective->fovy #} p
      aspect <- {# get ClutterPerspective->aspect #} p
      z_near <- {# get ClutterPerspective->z_near #} p
      z_far <- {# get ClutterPerspective->z_far #} p
      return $ Perspective (cFloatConv fovy) (cFloatConv aspect) (cFloatConv z_near) (cFloatConv z_far)

  poke p (Perspective fovy aspect z_near z_far) = do
      {# set ClutterPerspective->fovy #} p (cFloatConv fovy)
      {# set ClutterPerspective->aspect #} p (cFloatConv aspect)
      {# set ClutterPerspective->z_near #} p (cFloatConv z_near)
      {# set ClutterPerspective->z_far #} p (cFloatConv z_far)
      return ()

--This seems not right. But it seems to work.
mkPerspective :: Perspective -> IO PerspectivePtr
mkPerspective pst = do pptr <- (malloc :: IO PerspectivePtr)
                       poke pptr pst
                       return pptr

withPerspective :: Perspective -> (PerspectivePtr -> IO a) -> IO a
withPerspective pst = bracket (mkPerspective pst) free

-- ***************************************************************

-- *************************************************************** ClutterEvent

{# pointer *ClutterEvent as Event foreign newtype #}

-- ***************************************************************

-- *************************************************************** Animation

{# pointer *ClutterAnimation as Animation foreign newtype #}

class GObjectClass o => AnimationClass o
toAnimation :: AnimationClass o => o -> Animation
toAnimation = unsafeCastGObject . toGObject

--FIXME?? Is this OK, with casting? Not always true?
--FIXME: Name and convention for this type deal.
newAnimation:: Ptr Animation -> IO Animation
newAnimation a = makeNewGObject Animation $ return (castPtr a)

instance AnimationClass Animation
instance GObjectClass Animation where
  toGObject (Animation a) = mkGObject (castForeignPtr a)
  unsafeCastGObject (GObject o) = Animation (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Timeline

{# pointer *ClutterTimeline as Timeline foreign newtype #}

class GObjectClass o => TimelineClass o
toTimeline :: TimelineClass o => o -> Timeline
toTimeline = unsafeCastGObject . toGObject

newTimeline:: Ptr Timeline -> IO Timeline
newTimeline a = makeNewGObject Timeline $ return a

instance TimelineClass Timeline
instance GObjectClass Timeline where
  toGObject (Timeline t) = mkGObject (castForeignPtr t)
  unsafeCastGObject (GObject o) = Timeline (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Alpha

{# pointer *ClutterAlpha as Alpha foreign newtype #}

class GObjectClass o => AlphaClass o
toAlpha :: AlphaClass o => o -> Alpha
toAlpha = unsafeCastGObject . toGObject

newAlpha:: Ptr Alpha -> IO Alpha
newAlpha a = makeNewGObject Alpha $ return a

instance AlphaClass Alpha
instance GObjectClass Alpha where
  toGObject (Alpha a) = mkGObject (castForeignPtr a)
  unsafeCastGObject (GObject o) = Alpha (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Interval

{# pointer *ClutterInterval as Interval foreign newtype #}

class GObjectClass o => IntervalClass o
toInterval :: IntervalClass o => o -> Interval
toInterval = unsafeCastGObject . toGObject

newInterval a = makeNewGObject Interval $ return (castPtr a)

instance IntervalClass Interval
instance GObjectClass Interval where
  toGObject (Interval i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = Interval (castForeignPtr o)

-- ***************************************************************

