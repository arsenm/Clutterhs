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
             ExistentialQuantification,
             FlexibleInstances,
             OverlappingInstances #-}
{-# OPTIONS_HADDOCK prune #-}

#include <glib.h>
#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Animation (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Animation'
-- @

-- * Types
  AnimOp(..),

-- * Constructors
  animate,
  animateWithAlpha,
  animateWithTimeline,
  animationNew,

-- * Methods
  animationSetObject,
  animationGetObject,
  animationSetMode,
  animationGetMode,

  animationSetDuration,
  animationGetDuration,

  animationSetLoop,
  animationGetLoop,

  animationSetTimeline,
  animationGetTimeline,
  animationSetAlpha,
  animationGetAlpha,
  animationCompleted,
  animationBind,
  animationBindInterval,
  animationUpdateInterval,

  animationHasProperty,
  animationUnbindProperty,
  animationGetInterval,
  actorGetAnimation,
  actorAnimation,

-- * Attributes
  animationObject,
  animationMode,
  animationDuration,
  animationLoop,
  animationTimeline,
  animationAlpha


--TODO: Signals, also name conflicts with timeline
-- * Signals
--onCompleted,
--afterCompleted,
--completed,
--onStarted,
--afterStarted,
--started
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.StoreValue #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS

import Control.Arrow (second)
import Control.Monad (foldM_)

import System.Glib.GObject
import System.Glib.Attributes

{# fun unsafe animation_new as ^ { } -> `Animation' newAnimation* #}

{# fun unsafe animation_set_object as ^
       `GObjectClass obj' => { withAnimation* `Animation', withGObject* `obj' } -> `()' #}
{# fun unsafe animation_get_object as ^
       { withAnimation* `Animation' } -> `GObject' newGObject* #}

-- FIXME ?: Property issue since can set any gobject class, but can only get GObject back
--animationObject :: (GObjectClass obj) => Attr Animation obj
animationObject :: Attr Animation GObject
animationObject = newAttr animationGetObject animationSetObject

{# fun unsafe animation_set_mode as ^
       { withAnimation* `Animation', cFromEnum `AnimationMode' } -> `()' #}
{# fun unsafe animation_get_mode as ^ { withAnimation* `Animation' } -> `AnimationMode' cToEnum #}
animationMode :: Attr Animation AnimationMode
animationMode = newAttr animationGetMode animationSetMode

--CHECKME: Set a gint, get out a guint?
-- | Sets the duration of animation in milliseconds.
--
-- This function will set "alpha" and "timeline" if needed.
--
-- [@animation@] an 'Animation'
--
-- [@msecs@] the duration in milliseconds
--
-- * Since 1.0
--
{# fun unsafe animation_set_duration as ^ { withAnimation* `Animation', `Int' } -> `()' #}

-- | Retrieves the duration of animation, in milliseconds.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] the duration of the animation
--
-- * Since 1.0
--
{# fun unsafe animation_get_duration as ^ { withAnimation* `Animation' } -> `Int' #}

animationDuration :: Attr Animation Int
animationDuration = newAttr animationGetDuration animationSetDuration



-- | Sets the 'Timeline' used by animation.
--
-- [@animation@] an 'Animation'
--
-- [@timeline@] @Just@ a 'Timeline', or @Nothing@ to unset the current
--   'Timeline'
--
-- * Since 1.0
--
{# fun unsafe animation_set_timeline as ^
       { withAnimation* `Animation', withMaybeTimeline* `Maybe Timeline' } -> `()' #}


--CHECKME: Return Null?
-- | Retrieves the 'Timeline' used by animation
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] the timeline used by the animation.
--
-- * Since 1.0
--
{# fun unsafe animation_get_timeline as ^
       { withAnimation* `Animation' } -> `Maybe Timeline' maybeNewTimeline* #}

animationTimeline :: Attr Animation (Maybe Timeline)
animationTimeline = newAttr animationGetTimeline animationSetTimeline


--CHECKME: animation ownership and nothing?
-- | Sets alpha as the 'Alpha' used by animation.
--
-- If alpha is not @Nothing@, the 'Animation' will take ownership of
-- the 'Alpha' instance.
--
-- [@animation@] an 'Animation'
--
-- [@alpha@] @Just@ an 'Alpha', or @Nothing@ to unset the current
--   'Alpha'
--
-- * Since 1.0
--
{# fun unsafe animation_set_alpha as ^
       { withAnimation* `Animation', withMaybeAlpha* `Maybe Alpha' } -> `()' #}

-- | Retrieves the 'Alpha' used by animation.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] the alpha object used by the animation.
--
-- * Since 1.0
--
{# fun unsafe animation_get_alpha as ^
       { withAnimation* `Animation' } -> `Maybe Alpha' maybeNewAlpha* #}

-- | the alpha object used by the animation.
animationAlpha :: Attr Animation (Maybe Alpha)
animationAlpha = newAttr animationGetAlpha animationSetAlpha


--TODO: Link signal name, but I'm not fully commited to signal names yet
-- | Sets whether animation should loop over itself once finished.
--
-- A looping 'Animation' will not emit the "completed" signal when
-- finished.
--
-- This function will set "alpha" and "timeline" if needed.
--
-- [@animation@] an 'Animation'
--
-- [@loop@] @True@ if the animation should loop
--
-- * Since 1.0
--
{# fun unsafe animation_set_loop as ^ { withAnimation* `Animation', `Bool' } -> `()' #}

-- | Retrieves whether animation is looping.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] @True@ if the animation is looping
--
-- * Since 1.0
--
{# fun unsafe animation_get_loop as ^ { withAnimation* `Animation' } -> `Bool' #}

-- | Whether an animation should loop over itself once finished.
animationLoop :: Attr Animation Bool
animationLoop = newAttr animationGetLoop animationSetLoop


--CHECKME: Referencing
-- | Emits the ::completed signal on animation
--
-- [@animation@] an 'Animation'
--
-- * Since 1.0
--
{# fun animation_completed as ^ { withAnimation* `Animation' } -> `()' #}

--Says it returns the Animation as a convenience for language bindings.
--This is convenient to me how?
--CHECKME: Is this even proper to bind?
--The whole animation thing could be better.
--CHECKME: Get rid of the return?
-- | Adds a single property with name property_name to the animation
--   animation. For more information about animations, see 'animate'.
--
-- This method returns the animation primarily to make chained calls
-- convenient in language bindings.
--
--
-- [@animation@] an 'Animation'
--
-- [@property_name@] the property to control
--
-- [@final@] The final value of the property
--
-- [@Returns@] The animation itself
--
-- * Since 1.0
--
{# fun unsafe animation_bind as ^
   `(GenericValueClass final)' => { withAnimation* `Animation',
                                    `String',
                                    withGenericValue* `final'} ->
                                   `Animation' newAnimation* #}


-- | Binds interval to the property_name of the GObject attached to
--   animation. The 'Animation' will take ownership of the passed
--   'Interval'. For more information about animations, see 'animate'.
--
-- If you need to update the interval instance use
-- 'animationUpdateProperty' instead.
--
-- [@animation@] an 'Animation'
--
-- [@property_name@] the property to control
--
-- [@interval@] an 'Interval'
--
-- [@Returns@] The animation itself
--
-- * Since 1.0
--
{# fun unsafe animation_bind_interval as ^
   { withAnimation* `Animation',
     `String',
     withInterval* `Interval'} ->
     `Animation' newAnimation* #}


-- | Changes the interval for property_name. The 'Animation' will take
--   ownership of the passed 'Interval'.
--
-- [@animation@] an 'Animation'
--
-- [@property_name@] name of the property
--
-- [@interval@] a 'Interval'
--
-- * Since 1.0
--
{# fun unsafe animation_update_interval as ^
   { withAnimation* `Animation', `String', withInterval* `Interval'} -> `()' #}


-- | Checks whether animation is controlling property_name.
--
-- [@animation@] an 'Animation'
--
-- [@property_name@] name of the property
--
-- [@Returns@] @True@ if the property is animated by the 'Animation',
--   @False@ otherwise
--
-- * Since 1.0
--
{# fun unsafe animation_has_property as ^
       { withAnimation* `Animation', `String' } -> `Bool' #}

--CHECKME: unsafe?
-- | Removes property_name from the list of animated properties.
--
-- [@animation@] an 'Animation'
--
-- [@property_name@] name of the property
--
-- * Since 1.0
--
{# fun unsafe animation_unbind_property as ^
       { withAnimation* `Animation', `String' } -> `()' #}


-- | Retrieves the 'Interval' associated to property_name inside
--   animation.
--
-- [@animation@] an 'Animation'
--
-- [@property_name@] name of the property
--
-- [@Returns@] the 'Interval'
--
-- * Since 1.0
--
{# fun unsafe animation_get_interval as ^
   { withAnimation* `Animation', `String' } -> `Interval' newInterval* #}


-- | Retrieves the 'Animation' used by actor, if 'animate' has been
--   called on actor.
--
-- [@actor@] an Actor
--
-- [@Returns@] @Just@ an 'Animation', or @Nothing@. transfer none.
--
-- * Since 1.0
--
{# fun actor_get_animation as ^
       `(ActorClass a)' => { withActorClass* `a' } -> `Maybe Animation' maybeNewAnimation* #}

-- | Retrieves the 'Animation' used by actor, if 'animate' has been
--   called on actor.
actorAnimation :: (ActorClass actor) => ReadAttr actor (Maybe Animation)
actorAnimation = readAttr actorGetAnimation


onCompleted, afterCompleted :: Animation -> IO () -> IO (ConnectId Animation)
onCompleted = connect_NONE__NONE "completed" False
afterCompleted = connect_NONE__NONE "completed" True


-- | The ::completed signal is emitted once the animation has been completed.
--
-- The animation instance is guaranteed to be valid for the entire
-- duration of the signal emission chain.
--
-- * Since 1.0
--
completed :: Signal Animation (IO ())
completed = Signal (connect_NONE__NONE "completed")


onStarted, afterStarted :: Animation -> IO () -> IO (ConnectId Animation)
onStarted = connect_NONE__NONE "started" False
afterStarted = connect_NONE__NONE "started" True


-- | The ::started signal is emitted once the animation has been
--   started
--
-- * Since 1.0
--
started :: Signal Animation (IO ())
started = Signal (connect_NONE__NONE "started")

--CHECKME: I don't think it makes sense to allow ReadAttr, but does it
--make sense to allow WriteAttr?
--CHECKME: I feel like GenericValueClass can go away
--data AnimOp o = forall a b. ReadWriteAttr o a b :-> b
data AnimOp o = forall a b. (GenericValueClass b) => ReadWriteAttr o a b :-> b

infixr 0 :->

toListAnim :: (ActorClass o) => [AnimOp o] -> ([String], [GenericValue])
toListAnim = foldr step ([], [])
    where step (attr :-> val) (strs, vals) = (show attr:strs, toGenericValue val:vals)

animate :: (ActorClass actor) => actor -> AnimationMode -> Int -> [AnimOp actor] -> IO Animation
animate _ _ _ [] = error "Need arguments to animate"
animate actor mode duration us =
    let (names, gvals) = toListAnim us
        animatev = {# call unsafe actor_animatev #}  --CHECKME: unsafe?
        cmode = cFromEnum mode
        cdur = cIntConv duration
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
         withActorClass actor $ \actptr ->
           withArray gvals $ \gvPtr -> do
               ret <- animatev actptr cmode cdur (cIntConv len) strptr gvPtr
               foldM_ unsetOneGVal gvPtr gvals
               newAnimation ret

animateWithAlpha :: (ActorClass actor) => actor -> Alpha -> [AnimOp actor] -> IO Animation
animateWithAlpha _ _ [] = error "Need arguments to animate with alpha"
animateWithAlpha actor alpha us =
    let (names, gvals) = toListAnim us
        animatev = {# call unsafe actor_animate_with_alphav #}    --CHECKME: unsafe?
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withActorClass actor $ \actptr ->
          withAlpha alpha $ \alphptr ->
            withArray gvals $ \gvPtr -> do
              ret <- animatev actptr alphptr (cIntConv len) strptr gvPtr
              foldM_ unsetOneGVal gvPtr gvals
              newAnimation ret


animateWithTimeline :: (ActorClass actor) =>
                       actor
                       -> AnimationMode
                       -> Timeline
                       -> [AnimOp actor]
                       -> IO Animation
animateWithTimeline _ _ _ [] = error "Need arguments to animate with timeline"
animateWithTimeline actor mode tml us =
    let (names, gvals) = toListAnim us
        animatev = {# call unsafe actor_animate_with_timelinev #}    --CHECKME: unsafe?
        cmode = cFromEnum mode
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withActorClass actor $ \actptr ->
          withTimeline tml $ \tmlptr ->
            withArray gvals $ \gvPtr -> do
              ret <- animatev actptr cmode tmlptr (cIntConv len) strptr gvPtr
              foldM_ unsetOneGVal gvPtr gvals
              newAnimation ret

