-- -*-haskell-*-
--  Clutter Types
--
--  Author : Matthew Arsenault
--
--  Created: 5 Sep 2009
--
--  Copyright (C) 2009 Matthew Arsenault
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 3 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--

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
                                  newActor,

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
                                --constructStage,

                                  Container,
                                  ContainerClass,
                                  toContainer,
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
                                  newInterval,

                                  Fog(..),
                                  FogPtr,
                                  withFog,

                                  CairoTexture,
                                  CairoTextureClass,
                                  withCairoTexture,
                                  newCairoTexture,

                                  Media,
                                  MediaClass,
                                  withMedia,

                                  ChildMeta,
                                  ChildMetaClass,
                                  newChildMeta,

                                  CloneClass,
                                  Clone,
                                  withClone,
                                  newClone,

                                  BehaviourClass,
                                  Behaviour,
                                  withBehaviour,
                                  newBehaviour,
                                  BehaviourScaleClass,
                                  BehaviourScale,
                                  withBehaviourScale,
                                  newBehaviourScale,

                                  PathClass,
                                  Path,
                                  withPath,
                                  newPath,

                                  Group,
                                  GroupClass,
                                  newGroup,
                                  withGroup
                                 ) where

--FIXME: Conflict with EventType Nothing
import Prelude hiding (Nothing)

import C2HS hiding (newForeignPtr)
import System.Glib.GObject
import System.Glib.Flags
import Foreign.ForeignPtr hiding (newForeignPtr)
import System.Glib.FFI
import Control.Monad (liftM, when)
import Control.Exception (bracket)

--GTK uses the floating reference stuff
--this function is from gtk2hs, where they use the
--"ObjectClass" which isn't necessary, so I change to GObjectClass
--also why the flipped newForeignPtr there?
--from foreign.concurrent or something, and also flipped in System.Glib.FFI
--I don't understand why
--I also don't see the difference between this function, and
--constructNewGObject
--Actually yes I do. This one removes the floating reference, which
--we don't want. Get rid of the floating reference to get a normal reference
--clutter actors and gtk widgets have floating references
--TODO: Actors as objectclass in gtk so we don't have this function duplicated here
--but that's sort of confusing
makeNewObject :: GObjectClass obj =>
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewObject constr generator = do
  objPtr <- generator
  when (objPtr == nullPtr) (fail "makeNewObject: object is NULL")
  objectRefSink objPtr
  obj <- System.Glib.FFI.newForeignPtr objPtr objectUnref
  return $! constr obj


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

newActor :: (ActorClass actor) =>  Ptr actor -> IO Actor
newActor a = makeNewObject Actor $ return (castPtr a)

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
newRectangle a = makeNewObject Rectangle $ return (castPtr a)
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
newText a = makeNewObject Text $ return (castPtr a)

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

newGroup :: Ptr Actor -> IO Group
newGroup a = makeNewObject Group $ return (castPtr a)


instance GroupClass Group
instance ContainerClass Group
instance ActorClass Group
instance GObjectClass Group where
  toGObject (Group g) = mkGObject (castForeignPtr g)
  unsafeCastGObject (GObject o) = Group (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Container

{# pointer *ClutterContainer as Container foreign newtype #}

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
--newStage, constructStage:: Ptr Actor -> IO Stage
newStage :: Ptr Actor -> IO Stage
newStage a = makeNewObject Stage $ return (castPtr a)

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

--FIXME: DO animations and timelines etc. have floating references or not?
--They don't derive from actor, so I'm going to go with no

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

-- *************************************************************** Fog

{# pointer *ClutterFog as FogPtr -> Fog #}

data Fog = Fog { z_near :: Float,
                 z_far :: Float
               } deriving (Eq, Show)

instance Storable Fog where
  sizeOf _ = {# sizeof ClutterFog #}
  alignment _ = alignment (undefined :: Float)
  peek p = do
      znear <- {# get ClutterFog->z_near #} p
      zfar <- {# get ClutterFog->z_far #} p
      return $ Fog (cFloatConv znear) (cFloatConv zfar)
  poke p (Fog zn zf) = do
      {# set ClutterFog->z_near #} p (cFloatConv zn)
      {# set ClutterFog->z_far #} p (cFloatConv zf)
      return ()

--This seems not right. But it seems to work.
mkFog :: Fog -> IO FogPtr
mkFog col = do cptr <- (malloc :: IO FogPtr)
               poke cptr col
               return cptr

withFog :: Fog -> (FogPtr -> IO a) -> IO a
withFog col = bracket (mkFog col) free

-- ***************************************************************

-- *************************************************************** CairoTexture

{# pointer *ClutterCairoTexture as CairoTexture foreign newtype #}

class GObjectClass o => CairoTextureClass o
toCairoTexture :: CairoTextureClass o => o -> CairoTexture
toCairoTexture = unsafeCastGObject . toGObject

newCairoTexture a = makeNewGObject CairoTexture $ return (castPtr a)

instance CairoTextureClass CairoTexture
instance GObjectClass CairoTexture where
  toGObject (CairoTexture i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = CairoTexture (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Media

--FIXME: Doesn't derive from GObject just GInterface??
{# pointer *ClutterMedia as Media foreign newtype #}

class GObjectClass o => MediaClass o
toMedia :: MediaClass o => o -> Media
toMedia = unsafeCastGObject . toGObject

newMedia a = makeNewGObject Media $ return (castPtr a)

instance MediaClass Media
instance GObjectClass Media where
  toGObject (Media i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = Media (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** ChildMeta

{# pointer *ClutterChildMeta as ChildMeta foreign newtype #}

class GObjectClass o => ChildMetaClass o
toChildMeta :: ChildMetaClass o => o -> Media
toChildMeta = unsafeCastGObject . toGObject

newChildMeta a = makeNewGObject ChildMeta $ return (castPtr a)

instance ChildMetaClass ChildMeta
instance GObjectClass ChildMeta where
  toGObject (ChildMeta i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = ChildMeta (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Clone

{# pointer *ClutterClone as Clone foreign newtype #}

class GObjectClass o => CloneClass o
toClone :: CloneClass o => o -> Clone
toClone = unsafeCastGObject . toGObject

newClone a = makeNewObject Clone $ return (castPtr a)

instance CloneClass Clone
instance GObjectClass Clone where
  toGObject (Clone i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = Clone (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** Behaviour

{# pointer *ClutterBehaviour as Behaviour foreign newtype #}

class GObjectClass o => BehaviourClass o
toBehaviour :: BehaviourClass o => o -> Clone
toBehaviour = unsafeCastGObject . toGObject

newBehaviour a = makeNewObject Behaviour $ return (castPtr a)

instance BehaviourClass Behaviour
instance GObjectClass Behaviour where
  toGObject (Behaviour i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = Behaviour (castForeignPtr o)

-- ***************************************************************

-- *************************************************************** BehaviourScale

{# pointer *ClutterBehaviourScale as BehaviourScale foreign newtype #}

class GObjectClass o => BehaviourScaleClass o
toBehaviourScale :: BehaviourScaleClass o => o -> Clone
toBehaviourScale = unsafeCastGObject . toGObject

newBehaviourScale a = makeNewObject BehaviourScale $ return (castPtr a)

instance BehaviourScaleClass BehaviourScale
instance BehaviourClass BehaviourScale
instance GObjectClass BehaviourScale where
  toGObject (BehaviourScale i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = BehaviourScale (castForeignPtr o)

-- ***************************************************************


-- *************************************************************** Path

{# pointer *ClutterPath as Path foreign newtype #}

class GObjectClass o => PathClass o
toPath :: PathClass o => o -> Clone
toPath = unsafeCastGObject . toGObject

newPath a = makeNewObject Path $ return (castPtr a)

instance PathClass Path
instance GObjectClass Path where
  toGObject (Path i) = mkGObject (castForeignPtr i)
  unsafeCastGObject (GObject o) = Path (castForeignPtr o)

-- ***************************************************************

