-- -*-haskell-*-
--  Clutter Animation
--
--  Author : Matthew Arsenault
--
--  Created: 20 Sep 2009
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Animation (
                                      animationNew,
                                      animationSetObject,
                                      animationGetObject,
                                      animationObject,

                                      animationSetMode,
                                      animationGetMode,
                                      animationMode,

                                      animationSetDuration,
                                      animationGetDuration,
                                      animationDuration,

                                      animationSetLoop,
                                      animationGetLoop,
                                      animationLoop,

                                      animationSetTimeline,
                                      animationGetTimeline,
                                      animationTimeline,

                                    --animationSetAlpha,
                                    --animationGetAlpha,
                                    --animationAlpha,

                                      animationCompleted,
                                      animationBind,
                                    --animationBindInterval,
                                    --animationUpdateInterval,
                                    --animationHasProperty,
                                    --animationUnbindProperty,
                                    --animationGetInterval,

                                    --actorAnimate,
                                    --actorAnimateWithTimeline,
                                    --actorAnimateWithAlpha,
                                    --actorAnimatev,
                                    --actorAnimatevwithTimelinev,
                                    --actorAnimatevWithAlphav,
                                    --actorGetAnimation
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GValue

{# fun unsafe animation_new as ^ {} -> `Animation' newAnimation* #}


--withGObjectClass::GObjectClass o => o -> (Ptr GObject -> IO b) -> IO b
--FIXME: Why are things expecting Ptr ()? and not Ptr Type? I thought this was working before
--FIXME: This really should work with (Ptr GObject -> IO b) but it doesn't
--and I don't understand why
withGObject::GObjectClass o => o -> (Ptr () -> IO b) -> IO b
withGObject obj act = (withForeignPtr . unGObject . toGObject) obj $ \ptr -> act (castPtr ptr)
--withGObject obj = withForeignPtr . unGObject . toGObject

--yet another terrible thing?
outGObject a = makeNewGObject mkGObject $ return (castPtr a)

{# fun unsafe animation_set_object as ^
       `GObjectClass obj' => { withAnimation* `Animation', withGObject* `obj' } -> `()' #}
--FIXME: Is this right? Is there a better way to get something of right type out?
-- how to have polymorphic return type
{# fun unsafe animation_get_object as ^
       { withAnimation* `Animation' } -> `GObject' outGObject* #}

--this is the issue I still don't know how to deal with
--animationObject :: (GObjectClass obj) => Attr Animation obj
animationObject :: Attr Animation GObject
animationObject = newAttr animationGetObject animationSetObject


{# fun unsafe animation_set_mode as ^
       { withAnimation* `Animation', cFromEnum `AnimationMode' } -> `()' #}
--FIXME: Set an gint, get a guint? what? why?
{# fun unsafe animation_get_mode as ^ { withAnimation* `Animation' } -> `AnimationMode' cToEnum #}
animationMode :: Attr Animation AnimationMode
animationMode = newAttr animationGetMode animationSetMode

{# fun unsafe animation_set_duration as ^ { withAnimation* `Animation', `Int' } -> `()' #}
--FIXME: Set an gint, get a guint? what? why?
{# fun unsafe animation_get_duration as ^ { withAnimation* `Animation' } -> `Int' #}
animationDuration :: Attr Animation Int
animationDuration = newAttr animationGetDuration animationSetDuration

{# fun unsafe animation_set_loop as ^ { withAnimation* `Animation', `Bool' } -> `()' #}
{# fun unsafe animation_get_loop as ^ { withAnimation* `Animation' } -> `Bool' #}
animationLoop :: Attr Animation Bool
animationLoop = newAttr animationGetLoop animationSetLoop

{# fun animation_completed as ^ { withAnimation* `Animation' } -> `()' #}

{-
animate :: (ActorClass actor) => actor -> AnimationMode -> Int -> [Property]
animate actor mode tml props = do
-}

--{# fun animation_bind as ^
--       {withAnimation* `Animation', `String', withGValue* `GValue'} -> `Animation' newStage #}
--This shouldn't be this messy. Why do I need the casting? And use fun
--I'm still missing something about the wrapping stuff
--Peter says that won't work
animationBind:: Animation -> String -> GValue -> IO Animation
animationBind self name gval = do
  b <- withCString name $ \str ->
       withAnimation self $ \anptr ->
      {# call animation_bind #} anptr str (unGValue gval)
  newAnimation b

--I bet this won't work.
unGValue :: GValue -> Ptr ()
unGValue (GValue a) = castPtr a

{-
{# pointer *GValue newtype #}
newtype GValue = GValue (Ptr (GValue))
-}

{# fun unsafe animation_set_timeline as ^
       { withAnimation* `Animation', withTimeline* `Timeline' } -> `()' #}
{# fun unsafe animation_get_timeline as ^ { withAnimation* `Animation' } -> `Timeline' newTimeline* #}
animationTimeline :: Attr Animation Timeline
animationTimeline = newAttr animationGetTimeline animationSetTimeline

