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
             ExistentialQuantification #-}
{-# OPTIONS_HADDOCK prune #-}

#include <glib.h>
#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Animation (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Animation'
-- @

-- * Types
  Animation,
  AnimationMode(..),
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
--animationGetInterval,
  actorGetAnimation,
  actorAnimation,

-- * Attributes
  animationObject,
  animationMode,
  animationDuration,
  animationLoop,
  animationTimeline,
  animationAlpha


--TODO: Playable class
-- * Signals
--onCompleted,
--afterCompleted,
--completed,
--onStarted,
--afterStarted,
--started
  ) where

{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Timeline #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.StoreValue #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Prelude
import qualified Prelude as P
import Data.Maybe (maybe)

import Control.Monad (liftM, foldM_)

import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GValue




-- | Creates a new 'Animation' instance. You should set the GObject to
--   be animated using 'animationSetObject', set the duration with
--   'animationSetDuration' and the easing mode using
--   'animationSetMode'.
--
-- Use 'animationBind' or 'animationBindInterval' to define the
-- properties to be animated. The interval and the animated properties
-- can be updated at runtime.
--
-- The 'actorAnimate' and relative family of functions provide an easy
-- way to animate an 'Actor' and automatically manage the lifetime of
-- a 'Animation' instance, so you should consider using those
-- functions instead of manually creating an animation.
--
-- [@Returns@] the newly created 'Animation'
--
-- * Since 1.0
--
animationNew :: (GObjectClass obj) => IO (Animation obj)
animationNew = liftM (mkAnimation (undefined :: obj)) (newAnimationRaw =<< {# call unsafe animation_new #})



-- | Attaches animation to object. The old animation should not be
--   used after setting a new object.
--
-- [@animation@] an 'Animation'
--
-- [@object@] a GObject
--
-- [@Returns@] The new animation associated with object.
--
-- * Since 1.0
--
animationSetObject :: (GObjectClass obj) => Animation a -> obj -> IO ()
animationSetObject anim obj = withAnimation anim $ \animPtr ->
                                withGObject obj $ \objPtr -> do
                                  {# call unsafe animation_set_object #} animPtr objPtr


-- | Retrieves the GObject attached to animation.
--
-- [@animation@] an Animation
--
-- [@Returns@] a GObject
--
-- * Since 1.0
--
animationGetObject :: (GObjectClass obj) => Animation obj -> IO obj
animationGetObject anim = withAnimation anim $ \animPtr ->
  liftM unsafeCastGObject (newGObject =<< {# call unsafe animation_get_object #} animPtr)

-- | Sets the animation mode of animation. The animation mode is a
--   logical id, either coming from the ClutterAnimationMode
--   enumeration or the return value of clutter_alpha_register_func().
--
-- This function will also set 'alpha' if needed.
--
-- [@animation@] an 'Animation'
--
-- [@mode@] an animation mode logical id
--
-- * Since 1.0
--
{# fun unsafe animation_set_mode as ^
       { withAnimation* `Animation a', cFromEnum `AnimationMode' } -> `()' #}


-- | Retrieves the animation mode of animation, as set by
--   'animationSetMode'.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] the mode for the animation
--
-- * Since 1.0
--
{# fun unsafe animation_get_mode as ^ { withAnimation* `Animation a' } -> `AnimationMode' cToEnum #}


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
{# fun unsafe animation_set_duration as ^ { withAnimation* `Animation a', `Int' } -> `()' #}

-- | Retrieves the duration of animation, in milliseconds.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] the duration of the animation
--
-- * Since 1.0
--
{# fun unsafe animation_get_duration as ^ { withAnimation* `Animation a' } -> `Int' #}


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
       { withAnimation* `Animation a', withMaybeTimeline* `Maybe Timeline' } -> `()' #}


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
       { withAnimation* `Animation a' } -> `Maybe Timeline' maybeNewTimeline* #}


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
       { withAnimation* `Animation a', withMaybeAlpha* `Maybe Alpha' } -> `()' #}

-- | Retrieves the 'Alpha' used by animation.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] the alpha object used by the animation.
--
-- * Since 1.0
--
{# fun unsafe animation_get_alpha as ^
       { withAnimation* `Animation a' } -> `Maybe Alpha' maybeNewAlpha* #}


-- | Sets whether animation should loop over itself once finished.
--
-- A looping 'Animation' will not emit the 'completed' signal when
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
{# fun unsafe animation_set_loop as ^ { withAnimation* `Animation a', `Bool' } -> `()' #}

-- | Retrieves whether animation is looping.
--
-- [@animation@] an 'Animation'
--
-- [@Returns@] @True@ if the animation is looping
--
-- * Since 1.0
--
{# fun unsafe animation_get_loop as ^ { withAnimation* `Animation a' } -> `Bool' #}


--CHECKME: Referencing
-- | Emits the ::'completed' signal on animation
--
-- [@animation@] an 'Animation'
--
-- * Since 1.0
--
{# fun animation_completed as ^ { withAnimation* `Animation a' } -> `()' #}


--FIXME: Use AnimOp
--Says it returns the Animation as a convenience for language bindings.
--This is convenient to me how?
-- | Adds a single property with name property_name to the animation
--   animation. For more information about animations, see 'animate'.
--
-- This method returns the animation primarily to make chained calls
-- convenient in language bindings.
--
--
-- [@animation@] an 'Animation'
--
-- [@property@] the property to control and it's final value
--
-- [@Returns@] The animation itself
--
-- * Since 1.0
--
animationBind :: Animation a -> AnimOp b -> IO (Animation a)
animationBind anim (attr :-> val) = let func = {# call unsafe animation_bind #}
                                        str = P.show attr
                                    in withAnimation anim $ \animPtr ->
                                         withCString str $ \strPtr ->
                                          withGenericValue val $ \valPtr ->
                                            liftM (mkAnimation (undefined ::a))
                                                  (newAnimationRaw =<< func animPtr strPtr valPtr)


-- | Binds interval to the property_name of the GObject attached to
--   animation. The 'Animation' will take ownership of the passed
--   'Interval'. For more information about animations, see 'animate'.
--
-- If you need to update the interval instance use
-- 'animationUpdateProperty' instead.
--
-- [@animation@] an 'Animation'
--
-- [@property@] the property to control
--
-- [@interval@] an 'Interval'
--
-- [@Returns@] The animation itself
--
-- * Since 1.0
--
animationBindInterval :: (GObjectClass obj) => Animation obj -> Attr obj o -> Interval o -> IO (Animation obj)
animationBindInterval anim attr interval = let func = {# call unsafe animation_bind_interval #}
                                               str = P.show attr
                                           in withAnimation anim $ \animPtr ->
                                                withInterval interval $ \iPtr ->
                                                  withCString str $ \strPtr ->
                                                    liftM (mkAnimation (undefined :: obj)) (newAnimationRaw =<< func animPtr strPtr iPtr)


-- | Changes the interval for property.
--
-- [@animation@] an 'Animation'
--
-- [@property@] name of the property
--
-- [@interval@] a 'Interval'
--
-- * Since 1.0
--
animationUpdateInterval :: Animation a -> Attr a b -> Interval b -> IO ()
animationUpdateInterval anim attr interval = let func = {# call unsafe animation_update_interval #}
                                                 str = P.show attr
                                             in withAnimation anim $ \animPtr ->
                                                  withCString str $ \strPtr ->
                                                    withInterval interval $ \iPtr ->
                                                      func animPtr strPtr iPtr


-- | Checks whether animation is controlling property.
--
-- [@animation@] an 'Animation'
--
-- [@property@] the attribute
--
-- [@Returns@] @True@ if the property is animated by the 'Animation',
--   @False@ otherwise
--
-- * Since 1.0
--
animationHasProperty :: Animation a -> Attr a b -> IO Bool
animationHasProperty anim attr = withAnimation anim $ \animPtr ->
                                   withCString (P.show attr) $ \strPtr ->
                                     liftM cToBool $ {# call unsafe animation_has_property #} animPtr strPtr

--CHECKME: unsafe?
-- | Removes property from the list of animated properties.
--
-- [@animation@] an 'Animation'
--
-- [@property@] the attribute
--
-- * Since 1.0
--
animationUnbindProperty :: Animation a -> Attr a b -> IO ()
animationUnbindProperty anim attr = withAnimation anim $ \animPtr ->
                                      withCString (P.show attr) $ \strPtr ->
                                        {# call unsafe animation_unbind_property #} animPtr strPtr

--The type of the interval should be set when you create it, and
--should be the same as the type of the corresponding attribute, so
--I'm assuming that bad things aren't happening in clutter.  This
--needs a good testing.

-- | Retrieves the 'Interval' associated to property_name inside
--   animation.
--
-- [@animation@] an 'Animation'
--
-- [@property@] An attribute
--
-- [@Returns@] the 'Interval'
--
-- * Since 1.0
--
animationGetInterval :: (GObjectClass a) => Animation a -> Attr a b -> IO (Interval b)
animationGetInterval anim attr = let func = {# call unsafe animation_get_interval #}
                                     str = P.show attr
                                 in withAnimation anim $ \animPtr ->
                                      withCString str $ \strPtr ->
                                          liftM
                                            (mkInterval (undefined :: b))
                                            (newIntervalRaw =<< func animPtr strPtr)


-- | Retrieves the 'Animation' used by actor, if 'animate' has been
--   called on actor.
--
-- [@actor@] an Actor
--
-- [@Returns@] @Just@ an 'Animation', or @Nothing@. transfer none.
--
-- * Since 1.0
--
actorGetAnimation :: (ActorClass a) => a -> IO (Maybe (Animation a))
actorGetAnimation actor =  withActorClass actor $ \actorPtr -> do
  raw <- {# call unsafe actor_get_animation #} actorPtr
  if raw == nullPtr
     then return P.Nothing
     else newAnimationRaw raw >>= return . Just . mkAnimation (undefined :: a)


--onCompleted, afterCompleted :: (GObjectClass a) => Animation a -> IO () -> IO (ConnectId (Animation a))

-- | The ::'completed' signal is emitted once the animation has been
--   completed.
--
-- The animation instance is guaranteed to be valid for the entire
-- duration of the signal emission chain.
--
-- * Since 1.0
--
--completed :: (GObjectClass a) => Signal (Animation a) (IO ())

--onStarted, afterStarted :: (GObjectClass a) => Animation a -> IO () -> IO (ConnectId (Animation a))


-- | The ::started signal is emitted once the animation has been
--   started
--
-- * Since 1.0
--
--started :: (GObjectClass a) => Signal (Animation a) (IO ())

-- CHECKME? allow WriteAttr?  Also, Animatable class? Not all
--attributes animatable. Especially the convenient ones I added like
--position and size
--TODO: Rename to be more general, since other stuff uses it too
--data AnimOp o = forall a b. ReadWriteAttr o a b :-> b
data AnimOp o = forall a b. (GenericValueClass b) => ReadWriteAttr o a b :-> b

infixr 0 :->

--TODO: Rename these
toListAnim :: (ActorClass o) => [AnimOp o] -> ([String], [GenericValue])
toListAnim = foldr step ([], [])
    where step (attr :-> val) (strs, vals) = (show attr:strs, toGenericValue val:vals)



-- | Animates the given list of attributes of actor between the
--   current value for each property and a new final value. The
--   animation has a definite duration and a speed given by the mode.
--
--
-- * Warning
--
-- Unlike clutter_actor_animate(), this function will not allow you to
-- specify "signal::" names and callbacks.
--
-- [@actor@] an Actor
--
-- [@mode@] an animation mode logical id
--
-- [@duration@] duration of the animation, in milliseconds
--
-- [@anim ops@] A list of attributes associated with their final
-- values.
--
-- * Since 1.0
--
animate :: (ActorClass actor) => actor -> AnimationMode -> Int -> [AnimOp actor] -> IO (Animation actor)
animate _ _ _ [] = error "Need arguments to animate"
animate actor mode duration us =
    let (names, gvals) = toListAnim us
        animatev = {# call actor_animatev #}
        cmode = cFromEnum mode
        cdur = cIntConv duration
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
         withActorClass actor $ \actptr ->
           withArray gvals $ \gvPtr -> do
               ret <- animatev actptr cmode cdur (cIntConv len) strptr gvPtr
               foldM_ unsetOneGVal gvPtr gvals
               raw <- newAnimationRaw ret
               return (mkAnimation (undefined :: actor) raw)



-- | Animates the given list of properties of actor between the
--   current value for each property and a new final value. The
--   animation has a definite behaviour given by the passed alpha.
--
-- See 'animate' for further details.
--
-- This function is useful if you want to use an existing 'Alpha' to animate actor.
--
-- * Warning
--
-- Unlike clutter_actor_animate_with_alpha(), this function will not
-- allow you to specify "signal::" names and callbacks.
--
-- [@actor@] an Actor
--
-- [@alpha@] an Alpha
--
-- [@anim ops@] A list of attributes associated with their final
-- values.
--
-- * Since 1.0
--
animateWithAlpha :: (ActorClass actor) => actor -> Alpha -> [AnimOp actor] -> IO (Animation actor)
animateWithAlpha _ _ [] = error "Need arguments to animate with alpha"
animateWithAlpha actor alpha us =
    let (names, gvals) = toListAnim us
        animatev = {# call actor_animate_with_alphav #}
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withActorClass actor $ \actptr ->
          withAlpha alpha $ \alphptr ->
            withArray gvals $ \gvPtr -> do
              ret <- animatev actptr alphptr (cIntConv len) strptr gvPtr
              foldM_ unsetOneGVal gvPtr gvals
              raw <- newAnimationRaw ret
              return (mkAnimation (undefined :: actor) raw)


-- | Animates the given list of properties of actor between the
--   current value for each property and a new final value. The
--   animation has a definite duration given by timeline and a speed
--   given by the mode.
--
-- See 'animate for further details.
--
-- This function is useful if you want to use an existing timeline to
-- animate actor.
--
-- [@actor@] an Actor
--
-- [@mode@] an animation mode logical id
--
-- [@timeline@] a 'Timeline'
--
-- [@anim ops@] A list of attributes associated with their final
-- values.
--
-- * Since 1.0
--
animateWithTimeline :: (ActorClass actor) =>
                       actor
                       -> AnimationMode
                       -> Timeline
                       -> [AnimOp actor]
                       -> IO (Animation actor)
animateWithTimeline _ _ _ [] = error "Need arguments to animate with timeline"
animateWithTimeline actor mode tml us =
    let (names, gvals) = toListAnim us
        animatev = {# call actor_animate_with_timelinev #}
        cmode = cFromEnum mode
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withActorClass actor $ \actptr ->
          withTimeline tml $ \tmlptr ->
            withArray gvals $ \gvPtr -> do
              ret <- animatev actptr cmode tmlptr (cIntConv len) strptr gvPtr
              foldM_ unsetOneGVal gvPtr gvals
              raw <- newAnimationRaw ret
              return (mkAnimation (undefined :: actor) raw)


-- | the alpha object used by the animation.
--
-- * Since 1.0
--
animationAlpha :: Attr (Animation a) (Maybe Alpha)
animationAlpha = newNamedAttr "alpha" animationGetAlpha animationSetAlpha


-- | The duration of the animation, expressed in milliseconds.
--
-- Default value: 0
--
-- * Since 1.0
--
animationDuration :: Attr (Animation a) Int
animationDuration = newNamedAttr "duration" animationGetDuration animationSetDuration


-- | Whether the animation should loop.
--
-- Default value: @False@
--
-- * Since 1.0
--
animationLoop :: Attr (Animation a) Bool
animationLoop = newNamedAttr "loop" animationGetLoop animationSetLoop


-- | The animation mode, either a value from 'AnimationMode' or a
--   value returned by clutter_alpha_register_func(). The default
--   value is 'Linear'.
--
-- * Since 1.0
--
animationMode :: Attr (Animation a) AnimationMode
animationMode = newAttr animationGetMode animationSetMode

-- | Retrieves the 'Animation' used by actor, if 'animate' has been
--   called on actor.
actorAnimation :: (ActorClass actor) => ReadAttr actor (Maybe (Animation actor))
actorAnimation = readAttr actorGetAnimation

-- | Object to which the animation applies.
--
animationObject :: (GObjectClass a) => Attr (Animation a) a
animationObject = newNamedAttr "object" animationGetObject animationSetObject


-- | The ClutterTimeline used by the animation.
--
-- * Since 1.0
--
animationTimeline :: Attr (Animation a) (Maybe Timeline)
animationTimeline = newNamedAttr "timeline" animationGetTimeline animationSetTimeline


--CHECKME: Does it make sense to have start, stop, pause for
-- Animation, even though there might not be a set timeline?  If so,
-- does it make sense to just do nothing if one isn't set?
instance (GObjectClass a) => Playable (Animation a) where
  start a = animationGetTimeline a >>= maybe (return ()) start
  pause a = animationGetTimeline a >>= maybe (return ()) pause
  stop a = animationGetTimeline a >>= maybe (return ()) stop
  started = Signal (connect_NONE__NONE "started")
  onStarted = connect_NONE__NONE "started" False
  afterStarted = connect_NONE__NONE "started" True
  completed = Signal (connect_NONE__NONE "completed")
  onCompleted = connect_NONE__NONE "completed" False
  afterCompleted = connect_NONE__NONE "completed" True
  paused = Signal (connect_NONE__NONE "paused")
  onPaused = connect_NONE__NONE "paused" False
  afterPaused = connect_NONE__NONE "paused" True


