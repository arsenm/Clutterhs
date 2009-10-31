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

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Stage (
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
  --stageReadPixels,

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
{# import Graphics.UI.Clutter.Signals #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe stage_get_default as ^ {} -> `Stage' newStage* #}
{# fun unsafe stage_new as ^ {} -> `Stage' newStage* #}
{# fun unsafe stage_is_default as ^ { withStage* `Stage' } -> `Bool' #}

{# fun unsafe stage_set_color as ^ { withStage* `Stage', withColor* `Color' } -> `()' #}
{# fun unsafe stage_get_color as ^ { withStage* `Stage', alloca- `Color' peek*} -> `()' #}
stageColor :: Attr Stage Color
stageColor = newAttr stageGetColor stageSetColor


--I don't think I care about using StageClass since stage at the bottom.
{# fun unsafe stage_set_fullscreen as ^ { withStage* `Stage', `Bool'} -> `()' #}
{# fun unsafe stage_get_fullscreen as ^ { withStage* `Stage' } -> `Bool' #}
stageFullscreen :: Attr Stage Bool
stageFullscreen = newAttr stageGetFullscreen stageSetFullscreen

--TODO: Property??
{# fun unsafe stage_show_cursor as ^ { withStage* `Stage' } -> `()' #}
{# fun unsafe stage_hide_cursor as ^ { withStage* `Stage' } -> `()' #}

{# fun unsafe stage_get_actor_at_pos as ^
       { withStage* `Stage', cFromEnum `PickMode', `Int', `Int'} -> `Actor' newActor* #}

{# fun unsafe stage_ensure_current as ^ { withStage* `Stage' } -> `()' #}
{# fun unsafe stage_ensure_viewport as ^ { withStage* `Stage' } -> `()' #}
{# fun unsafe stage_ensure_redraw as ^ { withStage* `Stage' } -> `()' #}
{# fun unsafe stage_queue_redraw as ^ { withStage* `Stage' } -> `()' #}

--CHECKME this might fall under the category of low level event stuff we're not dealing with
--{# fun unsafe stage_event as ^ { withStage* `Stage', withEvent* `Event' } -> `Bool' #}
{# fun unsafe stage_set_key_focus as ^ `(ActorClass actor)' => { withStage* `Stage', withActorClass* `actor' } -> `()' #}
{# fun unsafe stage_get_key_focus as ^ { withStage* `Stage' } -> `Actor' newActor* #}
--TODO: Same problem as other places, setting and getting ActorClass is unhappy
--stageKeyFocus :: (ActorClass actor) => Attr Stage actor
--stageKeyFocus = newAttr stageGetKeyFocus stageSetKeyFocus

--TODO: all those types, namely guchar* out = what?
--Returns some kind of image buffer, what do I do with it?
--{# fun unsafe stage_read_pixels as ^ { withStage* `Stage', `Int', `Int', `Int', `Int' } -> `Ptr ()' #}

{# fun unsafe stage_set_throttle_motion_events as ^ { withStage* `Stage', `Bool' } -> `()' #}
{# fun unsafe stage_get_throttle_motion_events as ^ { withStage* `Stage' } -> `Bool' #}
stageThrottleMotionEvents :: Attr Stage Bool
stageThrottleMotionEvents = newAttr stageGetThrottleMotionEvents stageSetThrottleMotionEvents


{# fun unsafe stage_get_perspective as ^ { withStage* `Stage', alloca- `Perspective' peek* } -> `()' #}
{# fun unsafe stage_set_perspective as ^ { withStage* `Stage', withPerspective* `Perspective'} -> `()' #}
stagePerspective :: Attr Stage Perspective
stagePerspective = newAttr stageGetPerspective stageSetPerspective


--TODO: Unicode???
{# fun unsafe stage_set_title as ^ { withStage* `Stage', `String' } -> `()' #}
{# fun unsafe stage_get_title as ^ { withStage* `Stage' } -> `String' #}
stageTitle :: Attr Stage String
stageTitle = newAttr stageGetTitle stageSetTitle

{# fun unsafe stage_set_user_resizable as ^ { withStage* `Stage', `Bool' } -> `()' #}
{# fun unsafe stage_get_user_resizable as ^ { withStage* `Stage' } -> `Bool' #}
stageUserResizable :: Attr Stage Bool
stageUserResizable = newAttr stageGetUserResizable stageSetUserResizable

{# fun unsafe stage_set_use_fog as ^ { withStage* `Stage', `Bool' } -> `()' #}
{# fun unsafe stage_get_use_fog as ^ { withStage* `Stage' } -> `Bool' #}
stageUseFog :: Attr Stage Bool
stageUseFog = newAttr stageGetUseFog stageSetUseFog

{# fun unsafe stage_set_fog as ^ { withStage* `Stage', withFog* `Fog' } -> `()' #}
{# fun unsafe stage_get_fog as ^ { withStage* `Stage', alloca- `Fog' peek* } -> `()' #}
stageFog :: Attr Stage Fog
stageFog = newAttr stageGetFog stageSetFog


onActivate, afterActivate :: Stage -> IO () -> IO (ConnectId Stage)
onActivate = connect_NONE__NONE "activate" False
afterActivate = connect_NONE__NONE "activate" True

activate :: Signal Stage (IO ())
activate = Signal (connect_NONE__NONE "activate")


onDeactivate, afterDeactivate :: Stage -> IO () -> IO (ConnectId Stage)
onDeactivate = connect_NONE__NONE "deactivate" False
afterDeactivate = connect_NONE__NONE "deactivate" True

deactivate :: Signal Stage (IO ())
deactivate = Signal (connect_NONE__NONE "deactivate")


onFullscreen, afterFullscreen :: Stage -> IO () -> IO (ConnectId Stage)
onFullscreen = connect_NONE__NONE "fullscreen" False
afterFullscreen = connect_NONE__NONE "fullscreen" True

fullscreen :: Signal Stage (IO ())
fullscreen = Signal (connect_NONE__NONE "fullscreen")


onUnfullscreen, afterUnfullscreen :: Stage -> IO () -> IO (ConnectId Stage)
onUnfullscreen = connect_NONE__NONE "unfullscreen" False
afterUnfullscreen = connect_NONE__NONE "unfullscreen" True

unfullscreen :: Signal Stage (IO ())
unfullscreen = Signal (connect_NONE__NONE "unfullscreen")

