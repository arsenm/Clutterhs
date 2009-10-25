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
{-# LANGUAGE ForeignFunctionInterface,
             TypeSynonymInstances,
             FlexibleInstances,
             OverlappingInstances #-}

#include <glib.h>
#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}


module Graphics.UI.Clutter.Animation (
                                      animate,
                                      animateWithAlpha,
                                      animateWithTimeline,

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

                                      animationSetAlpha,
                                      animationGetAlpha,
                                      animationAlpha,

                                      animationCompleted,
                                    --animationBind,
                                      animationBindInterval,
                                      animationUpdateInterval,
                                      animationHasProperty,
                                      animationUnbindProperty,
                                      animationGetInterval,

                                      actorGetAnimation,
                                      actorAnimation
                                     ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.StoreValue #}

import C2HS

import Control.Arrow (second)
import Control.Monad (foldM_)

import System.Glib.GObject
import System.Glib.Attributes

--{# import Graphics.UI.Clutter.GValue #}

{# fun unsafe animation_new as ^ { } -> `Animation' newAnimation* #}

{# fun unsafe animation_set_object as ^
       `GObjectClass obj' => { withAnimation* `Animation', withGObject* `obj' } -> `()' #}
{# fun unsafe animation_get_object as ^
       { withAnimation* `Animation' } -> `GObject' newGObject* #}

--FIXME?: Property issue since can set any gobject class, but can only get GObject back
--animationObject :: (GObjectClass obj) => Attr Animation obj
animationObject :: Attr Animation GObject
animationObject = newAttr animationGetObject animationSetObject

{# fun unsafe animation_set_mode as ^
       { withAnimation* `Animation', cFromEnum `AnimationMode' } -> `()' #}
{# fun unsafe animation_get_mode as ^ { withAnimation* `Animation' } -> `AnimationMode' cToEnum #}
animationMode :: Attr Animation AnimationMode
animationMode = newAttr animationGetMode animationSetMode

--CHECKME: Set a gint, get out a guint?
{# fun unsafe animation_set_duration as ^ { withAnimation* `Animation', `Int' } -> `()' #}
{# fun unsafe animation_get_duration as ^ { withAnimation* `Animation' } -> `Int' #}
animationDuration :: Attr Animation Int
animationDuration = newAttr animationGetDuration animationSetDuration

{# fun unsafe animation_set_timeline as ^
       { withAnimation* `Animation', withTimeline* `Timeline' } -> `()' #}
{# fun unsafe animation_get_timeline as ^
       { withAnimation* `Animation' } -> `Timeline' newTimeline* #}
animationTimeline :: Attr Animation Timeline
animationTimeline = newAttr animationGetTimeline animationSetTimeline


{# fun unsafe animation_set_alpha as ^
       { withAnimation* `Animation', withAlpha* `Alpha' } -> `()' #}
{# fun unsafe animation_get_alpha as ^
       { withAnimation* `Animation' } -> `Alpha' newAlpha* #}
animationAlpha :: Attr Animation Alpha
animationAlpha = newAttr animationGetAlpha animationSetAlpha

{# fun unsafe animation_set_loop as ^ { withAnimation* `Animation', `Bool' } -> `()' #}
{# fun unsafe animation_get_loop as ^ { withAnimation* `Animation' } -> `Bool' #}
animationLoop :: Attr Animation Bool
animationLoop = newAttr animationGetLoop animationSetLoop


{# fun animation_completed as ^ { withAnimation* `Animation' } -> `()' #}

--This shouldn't be this messy. Why do I need the casting? And use fun
--I'm still missing something about the wrapping stuff
--Peter says that won't work
--TODO: Don't take gvalue directly, use gvaluearg
--Says it returns the Animation as a convenience for language bindings.
--This is convenient to me how?
{-
{# fun unsafe animation_bind as ^
   `(GValueArgClass final)' => { withAnimation* `Animation',
                                 `String',
                                 withGValueArg* `final'} ->
                                 `Animation' newAnimation* #}
-}
{# fun unsafe animation_bind_interval as ^
   { withAnimation* `Animation',
     `String',
     withInterval* `Interval'} ->
     `Animation' newAnimation* #}

{# fun unsafe animation_update_interval as ^
   { withAnimation* `Animation', `String', withInterval* `Interval'} -> `()' #}

{# fun unsafe animation_has_property as ^
       { withAnimation* `Animation', `String' } -> `Bool' #}

--CHECKME: unsafe?
{# fun unsafe animation_unbind_property as ^
       { withAnimation* `Animation', `String' } -> `()' #}

{# fun unsafe animation_get_interval as ^
   { withAnimation* `Animation', `String' } -> `Interval' newInterval* #}

{# fun actor_get_animation as ^
       `(ActorClass a)' => { withActorClass* `a' } -> `Animation' newAnimation* #}
actorAnimation :: (ActorClass actor) => ReadAttr actor Animation
actorAnimation = readAttr actorGetAnimation

--FIXME/TODO: Type for Duration, type Duration = UInt or whatever

animate :: (ActorClass actor, AnimateType r) => actor -> AnimationMode -> Int -> r
animate actor mode duration = runAnim actor mode duration []

animateWithAlpha :: (ActorClass actor, AnimateType r) => actor -> Alpha -> r
animateWithAlpha actor alpha = runAnimWithAlpha actor alpha []

animateWithTimeline :: (ActorClass actor, AnimateType r) => actor -> AnimationMode -> Timeline -> r
animateWithTimeline actor mode tml = runAnimWithTimeline actor mode tml []


class AnimateType t where
    runAnim :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, GenericValue)] -> t
    runAnimWithAlpha :: (ActorClass actor) => actor -> Alpha -> [(String, GenericValue)] -> t
    runAnimWithTimeline :: (ActorClass actor) =>
                           actor ->
                           AnimationMode ->
                           Timeline ->
                           [(String, GenericValue)] ->
                           t

-- Multiple return types isn't useful. We only want IO Animation.
-- This breaks things if you remove the instance for ().
-- Also type inference not working in many cases. maybe find a better way
instance AnimateType (IO Animation) where
    runAnim actor mode duration args = uanimate actor mode duration args
    runAnimWithAlpha actor alpha args = uanimateWithAlpha actor alpha args
    runAnimWithTimeline actor mode tml args = uanimateWithTimeline actor mode tml args

instance AnimateType (IO ()) where
    runAnim actor mode duration args = uanimate actor mode duration args >> return ()
    runAnimWithAlpha actor alpha args = uanimateWithAlpha actor alpha args >> return ()
    runAnimWithTimeline actor mode tml args = uanimateWithTimeline actor mode tml args >> return ()

instance (AnimateArg a, AnimateType r) => AnimateType (a -> r) where
    runAnim actor mode duration args = \a -> runAnim actor mode duration (toAnimateArg a : args)
    runAnimWithAlpha actor alpha args = \a -> runAnimWithAlpha actor alpha (toAnimateArg a : args)
    runAnimWithTimeline actor mode tml args = \a -> runAnimWithTimeline actor mode tml (toAnimateArg a : args)

uanimate :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, GenericValue)] -> IO Animation
uanimate _ _ _ [] = error "Need arguments to animate"
uanimate actor mode duration us =
    let (names, gvals) = unzip us
        animatev = {# call unsafe actor_animatev #}  --CHECKME: unsafe?
        cmode = cFromEnum mode
        cdur = cIntConv duration
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
         withActorClass actor $ \actptr ->
           withArray gvals $ \gvPtr -> do
               ret <- animatev actptr cmode cdur (cIntConv len) strptr (castPtr gvPtr)
               foldM_ unsetOneGVal gvPtr gvals
               newAnimation ret

uanimateWithAlpha :: (ActorClass actor) => actor -> Alpha -> [(String, GenericValue)] -> IO Animation
uanimateWithAlpha _ _ [] = error "Need arguments to animate with alpha"
uanimateWithAlpha actor alpha us =
    let (names, gvals) = unzip us
        animatev = {# call unsafe actor_animate_with_alphav #}    --CHECKME: unsafe?
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withActorClass actor $ \actptr ->
          withAlpha alpha $ \alphptr ->
            withArray gvals $ \gvPtr -> do
              ret <- animatev actptr alphptr (cIntConv len) strptr (castPtr gvPtr)
              foldM_ unsetOneGVal gvPtr gvals
              newAnimation ret


uanimateWithTimeline :: (ActorClass actor) =>
                        actor ->
                        AnimationMode ->
                        Timeline ->
                        [(String, GenericValue)] ->
                        IO Animation
uanimateWithTimeline _ _ _ [] = error "Need arguments to animate with timeline"
uanimateWithTimeline actor mode tml us =
    let (names, gvals) = unzip us
        animatev = {# call unsafe actor_animate_with_timelinev #}    --CHECKME: unsafe?
        cmode = cFromEnum mode
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withActorClass actor $ \actptr ->
          withTimeline tml $ \tmlptr ->
            withArray gvals $ \gvPtr -> do
              ret <- animatev actptr cmode tmlptr (cIntConv len) strptr (castPtr gvPtr)
              foldM_ unsetOneGVal gvPtr gvals
              newAnimation ret

class AnimateArg a where
    toAnimateArg :: a -> (String, GenericValue)

instance AnimateArg (String, String) where
    toAnimateArg = second (GVstring . Just)
instance AnimateArg (String, Int) where
    toAnimateArg = second toGenericValue
instance AnimateArg (String, Float) where
    toAnimateArg = second GVfloat
instance AnimateArg (String, Color) where
    toAnimateArg = second GVcolor
instance AnimateArg (String, Double) where
    toAnimateArg = second GVdouble
instance AnimateArg (String, Word8) where
    toAnimateArg = second GVuchar

--CHECKME: OverlappingInstances
--TODO: Maybe just list all known gobjects we want to allow?
--that seems bad...
instance (GObjectClass obj) => AnimateArg (String, obj) where
    toAnimateArg = second (GVobject . toGObject)

