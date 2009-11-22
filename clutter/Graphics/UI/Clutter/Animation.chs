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
             ScopedTypeVariables,
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
import Prelude
import qualified Prelude as P

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
--{# fun unsafe animation_new as ^ { } -> `Animation' newAnimation* #}
animationNew :: IO (Animation ())
animationNew = liftM (mkAnimation (undefined :: ())) (newAnimationRaw =<< {# call unsafe animation_new #})

--CHECKME: Not using the old animation?
--CHECKME: Referencing. Creating the "new" animation using the raw animation
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
animationSetObject :: (GObjectClass obj) => Animation a -> obj -> IO (Animation obj)
animationSetObject anim obj = withAnimation anim $ \animPtr ->
                                withGObject obj $ \objPtr -> do
                                  {# call unsafe animation_set_object #} animPtr objPtr
                                  return $ mkAnimation (undefined::obj) (getAnimationRaw anim)



--CHECKME: You should be able to create an empty animation without an
--object, which will be of type Animation (), which you won't be
--allowed to get the object from with this.

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
-- [@property_name@] the property to control
--
-- [@final@] The final value of the property
--
-- [@Returns@] The animation itself
--
-- * Since 1.0
--
{# fun unsafe animation_bind as ^
   `(GenericValueClass final)' => { withAnimation* `Animation a',
                                    `String',
                                    withGenericValue* `final'} ->
                                   `AnimationRaw' newAnimationRaw* #}


--CHECKME: Wrap animation type to track type of object animated?
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
animationBindInterval :: (GObjectClass obj) => Animation obj -> Attr obj o -> Interval o -> IO (Animation obj)
animationBindInterval anim attr interval = let func = {# call unsafe animation_bind_interval #}
                                               str = P.show attr
                                           in withAnimation anim $ \animPtr ->
                                                withInterval interval $ \iPtr ->
                                                  withCString str $ \strPtr ->
                                                    liftM (mkAnimation (undefined :: obj)) (newAnimationRaw =<< func animPtr strPtr iPtr)


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
   { withAnimation* `Animation a', `String', withInterval* `Interval a'} -> `()' #}


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
       { withAnimation* `Animation a', `String' } -> `Bool' #}

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
       { withAnimation* `Animation a', `String' } -> `()' #}


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


onCompleted, afterCompleted :: (GObjectClass a) => Animation a -> IO () -> IO (ConnectId (Animation a))
onCompleted = connect_NONE__NONE "completed" False
afterCompleted = connect_NONE__NONE "completed" True


-- | The ::'completed' signal is emitted once the animation has been
--   completed.
--
-- The animation instance is guaranteed to be valid for the entire
-- duration of the signal emission chain.
--
-- * Since 1.0
--
completed :: (GObjectClass a) => Signal (Animation a) (IO ())
completed = Signal (connect_NONE__NONE "completed")


onStarted, afterStarted :: (GObjectClass a) => Animation a -> IO () -> IO (ConnectId (Animation a))
onStarted = connect_NONE__NONE "started" False
afterStarted = connect_NONE__NONE "started" True


-- | The ::started signal is emitted once the animation has been
--   started
--
-- * Since 1.0
--
started :: (GObjectClass a) => Signal (Animation a) (IO ())
started = Signal (connect_NONE__NONE "started")


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
        animatev = {# call unsafe actor_animate_with_alphav #}    --CHECKME: unsafe?
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

--CHECKME: You can get a different type of animation from setting back
--which you would then use, so I'm not sure this property entirely
--makes sense as writable. It could only possibly make sense if you set an object
--of the same type.  Is it a problem to not have this?

-- | Object to which the animation applies.
--
-- * Note
--
--  This is not writable since you may set a different type of object,
--  and must use the result of the set operation as a new animation
--  Use 'animationSetObject' and use the result instead.
--
animationObject :: (GObjectClass a) => ReadAttr (Animation a) a
animationObject = readNamedAttr "object" animationGetObject


-- | The ClutterTimeline used by the animation.
--
-- * Since 1.0
--
animationTimeline :: Attr (Animation a) (Maybe Timeline)
animationTimeline = newNamedAttr "timeline" animationGetTimeline animationSetTimeline


