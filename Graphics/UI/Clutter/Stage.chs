-- -*-haskell-*-
--  Clutter Stage
--
--  Author : Matthew Arsenault
--
--  Created: 11 Sep 2009
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
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

--FIXME: remove reference to freeing stuff
-- | 'Stage' — Top level visual element to which actors are placed.
module Graphics.UI.Clutter.Stage (
-- * Detail
-- | ClutterStage is a top level 'window' on which child actors are placed and manipulated.
--
-- Clutter creates a default stage upon initialization, which can be
-- retrieved using 'stageGetDefault'. Clutter always
-- provides the default stage, unless the backend is unable to create
-- one. The stage returned by 'stageGetDefault' is
-- guaranteed to always be the same.
--
-- Backends might provide support for multiple stages. The support for
-- this feature can be checked at run-time using the
-- 'clutterFeatureAvailable' function and the
-- CLUTTER_FEATURE_STAGE_MULTIPLE flag. If the backend used supports
-- multiple stages, new 'Stage' instances can be created using
-- 'stageNew'. These stages must be managed by the developer
-- using 'actorDestroy', which will take care of destroying
-- all the actors contained inside them.
--
-- 'Stage' is a proxy actor, wrapping the backend-specific
-- implementation of the windowing system. It is possible to subclass
-- 'Stage', as long as every overridden virtual function chains up
-- to the parent class corresponding function.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GInitiallyUnowned'
-- |         +----'Actor'
-- |               +----'Group'
-- |                     +----'Stage'
-- @

-- * Constructors
  stageGetDefault,
  stageNew,

  -- * Methods,
  stageIsDefault,

  stageSetColor,
  stageGetColor,

  stageSetFullscreen,
  stageGetFullscreen,

  stageShowCursor,
  stageHideCursor,

  stageGetActorAtPos,

  stageEnsureCurrent,
  stageEnsureViewport,

  stageEnsureRedraw,
  stageQueueRedraw,

  --stageEvent,
  stageSetKeyFocus,
  stageGetKeyFocus,
  --stageKeyFocus,
  stageReadPixels,

  stageSetThrottleMotionEvents,
  stageGetThrottleMotionEvents,

  stageSetPerspective,
  stageGetPerspective,

  stageSetTitle,
  stageGetTitle,

  stageSetUserResizable,
  stageGetUserResizable,

  stageSetUseFog,
  stageGetUseFog,

  stageSetFog,
  stageGetFog,

-- * Attributes
  stageColor,
  stageFullscreen,
  stageThrottleMotionEvents,
  stagePerspective,
  stageTitle,
  stageUserResizable,

  stageUseFog,
  stageFog,

-- * Signals
--FIXME: Export conflicts with Text's signals and probably other signals
--onActivate,
--afterActivate,
--activate,

  onDeactivate,
  afterDeactivate,
  deactivate,

  onFullscreen,
  afterFullscreen,
  fullscreen,

  onUnfullscreen,
  afterUnfullscreen,
  unfullscreen
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Actor #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.FFI (maybeNull)

-- | Returns the main stage. The default 'Stage' is a singleton, so
--   the stage will be created the first time this function is called
--   (typically, inside 'clutterInit'); all the subsequent calls to
--   'stageGetDefault' will return the same instance.
--   Clutter guarantess the existence of the default stage.
{# fun unsafe stage_get_default as ^ { } -> `Stage' newStage* #}

--CHECKME: Description says can return Null, but I don't check for that
--Also talking about pointer = no
-- | Creates a new, non-default stage. A non-default stage is a new
--   top-level actor which can be used as another container. It works
--   exactly like the default stage, but while
--   'stageGetDefault' will always return the same instance,
--   you will have to keep a pointer to any ClutterStage returned by
--   stageNew.
--
--   The ability to support multiple stages depends on the current
--   backend. Use 'clutterFeatureAvailable' and
--   CLUTTER_FEATURE_STAGE_MULTIPLE to check at runtime whether a backend
--   supports multiple stages.
--
--  [@Returns@]
--   a new stage, or NULL if the default backend does not support
--   multiple stages. Use clutter_actor_destroy() to programmatically
--   close the returned stage.
{# fun unsafe stage_new as ^ { } -> `Stage' newStage* #}

-- |Checks if stage is the default stage, or an instance created using
--  'stageNew' but internally using the same implementation.
{# fun unsafe stage_is_default as ^ { withStage* `Stage' } -> `Bool' #}

-- | Sets the stage color.
{# fun unsafe stage_set_color as ^ { withStage* `Stage', withColor* `Color' } -> `()' #}
-- | Retrieves the stage color.
{# fun unsafe stage_get_color as ^ { withStage* `Stage', alloca- `Color' peek*} -> `()' #}
stageColor :: Attr Stage Color
stageColor = newAttr stageGetColor stageSetColor


--I don't think I care about using StageClass since stage at the bottom.

-- | Asks to place the stage window in the fullscreen or unfullscreen
--  states.
--
-- ( Note that you shouldn't assume the window is definitely full screen
--  afterward, because other entities (e.g. the user or window manager)
--  could unfullscreen it again, and not all window managers honor
--  requests to fullscreen windows.
--
--  If you want to receive notification of the fullscreen state you should
--  either use the "fullscreen" and "unfullscreen" signals, or use the
--  notify signal for the "fullscreen-set" property
{# fun unsafe stage_set_fullscreen as ^ { withStage* `Stage', `Bool'} -> `()' #}
-- | Retrieves whether the stage is full screen or not
{# fun unsafe stage_get_fullscreen as ^ { withStage* `Stage' } -> `Bool' #}
stageFullscreen :: Attr Stage Bool
stageFullscreen = newAttr stageGetFullscreen stageSetFullscreen

--TODO: Property??
-- | Shows the cursor on the stage window
{# fun unsafe stage_show_cursor as ^ { withStage* `Stage' } -> `()' #}
-- | Makes the cursor invisible on the stage window
{# fun unsafe stage_hide_cursor as ^ { withStage* `Stage' } -> `()' #}

--CHECKME: Should I even include non-application functions?

-- | Checks the scene at the coordinates x and y and returns a pointer to the 'Actor' at those coordinates.
--
--  By using pick_mode it is possible to control which actors will be painted and thus available.
{# fun unsafe stage_get_actor_at_pos as ^
       { withStage* `Stage', cFromEnum `PickMode', `Int', `Int'} -> `Actor' newActor* #}

-- | This function essentially makes sure the right GL context is
-- | current for the passed stage. It is not intended to be used by
-- | applications.
{# fun unsafe stage_ensure_current as ^ { withStage* `Stage' } -> `()' #}

-- | Ensures that the GL viewport is updated with the current stage
--   window size.
--
-- This function will queue a redraw of stage.
--
-- This function should not be called by applications; it is used when
-- embedding a ClutterStage into a toolkit with another windowing
-- system, like GTK+.
{# fun unsafe stage_ensure_viewport as ^ { withStage* `Stage' } -> `()' #}

-- | Ensures that stage is redrawn
--
-- This function should not be called by applications: it is used when
-- embedding a 'Stage' into a toolkit with another windowing
-- system, like GTK+.
{# fun unsafe stage_ensure_redraw as ^ { withStage* `Stage' } -> `()' #}

-- | Queues a redraw for the passed stage.
--
-- Note
--
-- Applications should call 'actorQueueRedraw' and not this function.
-- Note
--
-- This function is just a wrapper for 'actorQueueRedraw' and should probably go away.
{# fun unsafe stage_queue_redraw as ^ { withStage* `Stage' } -> `()' #}

--CHECKME this might fall under the category of low level event stuff we're not dealing with
--{# fun unsafe stage_event as ^ { withStage* `Stage', withEvent* `Event' } -> `Bool' #}


-- | Sets the key focus on actor. An actor with key focus will receive
--   all the key events. If actor is @Nothing@, the stage will receive
--   focus.
--
-- [@stage@] the 'Stage'
--
-- [@actor@] @Just@ the actor to set key focus to, or @Nothing@
--
-- * Since 0.6
--
{# fun unsafe stage_set_key_focus as ^ `(ActorClass actor)' =>
       { withStage* `Stage', withMaybeActorClass* `Maybe actor' } -> `()' #}

-- | Retrieves the actor that is currently under key focus.
--
-- [@stage@] the 'Stage'
--
-- [@Returns@] the actor with key focus, or the stage. transfer none.
--
-- * Since 0.6
--
{# fun unsafe stage_get_key_focus as ^ { withStage* `Stage' } -> `Actor' newActor* #}

--TODO: Same problem as other places, setting and getting ActorClass is unhappy
--stageKeyFocus :: (ActorClass actor) => Attr Stage actor
--stageKeyFocus = newAttr stageGetKeyFocus stageSetKeyFocus

--TODO: all those types, namely guchar* out = what?
--Returns some kind of image buffer, what do I do with it?
--{# fun unsafe stage_read_pixels as ^ { withStage* `Stage', `Int', `Int', `Int', `Int' } -> `Ptr ()' #}
--Why is this scattered around in many places in gtk2hs?
foreign import ccall unsafe "&g_free"
  finalizerGFree :: FinalizerPtr a


stageReadPixels :: Stage -> Int -> Int -> Int -> Int -> IO (Maybe (RGBData Int Word8))
stageReadPixels stage x y w h = let cx = cIntConv x
                                    cy = cIntConv y
                                    cw = cIntConv w
                                    ch = cIntConv h
                                in withStage stage $ \stgPtr -> do
                                  sizeW <- if w == -1
                                              then liftM floor (actorGetWidth stage)
                                              else return w
                                  sizeH <- if h == -1
                                              then liftM floor (actorGetHeight stage)
                                              else return h
                                  let size = sizeW * sizeH * 4
                                  ptr <- {# call unsafe stage_read_pixels #} stgPtr cx cy cw ch
                                  if ptr == nullPtr
                                     then return Prelude.Nothing
                                     else newForeignPtr finalizerGFree ptr >>= \fptr ->
                                            return $ Just (mkRGBData (castForeignPtr fptr) True size)


-- | Sets whether motion events received between redraws should be
-- throttled or not. If motion events are throttled, those events
-- received by the windowing system between redraws will be compressed
-- so that only the last event will be propagated to the stage and its
-- actors.  This function should only be used if you want to have all
-- the motion events delivered to your application code.
{# fun unsafe stage_set_throttle_motion_events as ^ { withStage* `Stage', `Bool' } -> `()' #}

-- | Retrieves the value set with 'stageSetThrottleMotionEvents'
{# fun unsafe stage_get_throttle_motion_events as ^ { withStage* `Stage' } -> `Bool' #}
stageThrottleMotionEvents :: Attr Stage Bool
stageThrottleMotionEvents = newAttr stageGetThrottleMotionEvents stageSetThrottleMotionEvents

-- | Retrieves the stage perspective.
{# fun unsafe stage_get_perspective as ^ { withStage* `Stage', alloca- `Perspective' peek* } -> `()' #}
-- | Sets the stage perspective.
{# fun unsafe stage_set_perspective as ^ { withStage* `Stage', withPerspective* `Perspective'} -> `()' #}
stagePerspective :: Attr Stage Perspective
stagePerspective = newAttr stageGetPerspective stageSetPerspective


--TODO: Unicode???
-- | Sets the stage title.
{# fun unsafe stage_set_title as ^ { withStage* `Stage', `String' } -> `()' #}
-- | Gets the stage title.
{# fun unsafe stage_get_title as ^ { withStage* `Stage' } -> `String' #}
stageTitle :: Attr Stage String
stageTitle = newAttr stageGetTitle stageSetTitle

-- | Sets if the stage is resizable by user interaction (e.g. via window manager controls)
{# fun unsafe stage_set_user_resizable as ^ { withStage* `Stage', `Bool' } -> `()' #}
-- | Retrieves the value set with 'stageSetUserResizable'.
{# fun unsafe stage_get_user_resizable as ^ { withStage* `Stage' } -> `Bool' #}
stageUserResizable :: Attr Stage Bool
stageUserResizable = newAttr stageGetUserResizable stageSetUserResizable


-- | Sets whether the depth cueing effect on the stage should be enabled or not.
--
--   Depth cueing is a 3D effect that makes actors farther away from
--   the viewing point less opaque, by fading them with the stage
--   color.
--
-- The parameters of the GL fog used can be changed using the 'stageSetFog' function.
{# fun unsafe stage_set_use_fog as ^ { withStage* `Stage', `Bool' } -> `()' #}

-- | Gets whether the depth cueing effect is enabled on stage.
{# fun unsafe stage_get_use_fog as ^ { withStage* `Stage' } -> `Bool' #}
stageUseFog :: Attr Stage Bool
stageUseFog = newAttr stageGetUseFog stageSetUseFog

-- | Sets the fog (also known as "depth cueing") settings for the stage.
--
--  A 'Stage' will only use a linear fog progression, which depends
--  solely on the distance from the viewer. The cogl_set_fog() function in
--  COGL exposes more of the underlying implementation, and allows
--  changing the for progression function. It can be directly used by
--  disabling the "use-fog" property and connecting a signal handler to
--  the "paint" signal on the stage, like:
-- TODO: The equivalent example
--
--  Note: The fogging functions only work correctly when the visible
--  actors use unmultiplied alpha colors. By default Cogl will premultiply
--  textures and cogl_set_source_color will premultiply colors, so unless
--  you explicitly load your textures requesting an unmultiplied
--  internal_format and use cogl_material_set_color you can only use
--  fogging with fully opaque actors.
--
-- We can look to improve this in the future when we can depend on
--  fragment shaders.
{# fun unsafe stage_set_fog as ^ { withStage* `Stage', withFog* `Fog' } -> `()' #}

-- | Retrieves the current depth cueing settings from the stage.
{# fun unsafe stage_get_fog as ^ { withStage* `Stage', alloca- `Fog' peek* } -> `()' #}
stageFog :: Attr Stage Fog
stageFog = newAttr stageGetFog stageSetFog



onActivate, afterActivate :: Stage -> IO () -> IO (ConnectId Stage)
onActivate = connect_NONE__NONE "activate" False
afterActivate = connect_NONE__NONE "activate" True

-- | The 'activate' signal is emitted when the stage receives key focus from the underlying window system.
activate :: Signal Stage (IO ())
activate = Signal (connect_NONE__NONE "activate")


onDeactivate, afterDeactivate :: Stage -> IO () -> IO (ConnectId Stage)
onDeactivate = connect_NONE__NONE "deactivate" False
afterDeactivate = connect_NONE__NONE "deactivate" True

-- | The 'deactivate' signal is emitted when the stage loses key focus from the underlying window system.
deactivate :: Signal Stage (IO ())
deactivate = Signal (connect_NONE__NONE "deactivate")


onFullscreen, afterFullscreen :: Stage -> IO () -> IO (ConnectId Stage)
onFullscreen = connect_NONE__NONE "fullscreen" False
afterFullscreen = connect_NONE__NONE "fullscreen" True

-- | The 'fullscreen' signal is emitted when the stage is made fullscreen.
fullscreen :: Signal Stage (IO ())
fullscreen = Signal (connect_NONE__NONE "fullscreen")


onUnfullscreen, afterUnfullscreen :: Stage -> IO () -> IO (ConnectId Stage)
onUnfullscreen = connect_NONE__NONE "unfullscreen" False
afterUnfullscreen = connect_NONE__NONE "unfullscreen" True

-- | The 'unfullscreen' signal is emitted when the stage leaves a fullscreen state.
unfullscreen :: Signal Stage (IO ())
unfullscreen = Signal (connect_NONE__NONE "unfullscreen")

