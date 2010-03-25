-- -*-haskell-*-
--  Clutter Stage
--
--  Author : Matthew Arsenault
--
--  Created: 11 Sep 2009
--
--  Copyright (C) 2009-2010 Matthew Arsenault
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

-- | 'Stage' — Top level visual element to which actors are placed.
module Graphics.UI.Clutter.Stage (
-- * Detail
-- | 'Stage' is a top level 'window' on which child actors are placed
-- and manipulated.
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
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'Group'
-- |                  +----'Stage'
-- @

-- * Types
  Stage,
  StageClass,
  Perspective(..),
  Fog(..),
  PickMode(..),

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
  stageCursorVisible,
  stageFog,
  stageFullscreenSet,
  stageOffscreen,
  stagePerspective,
  stageTitle,
  stageUseFog,
  stageUserResizable,

-- * Signals
--FIXME: Export conflicts with Text's signals and probably other signals
  activate,
  deactivate,
  fullscreen,
  unfullscreen
  ) where

{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Actor #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import Data.Array.MArray
import Data.Array.Storable
--import Data.Ix
import System.Glib.Attributes
import System.Glib.Properties

-- debugging, this shouldn't still be here.
import GHC.Arr

-- | Returns the main stage. The default 'Stage' is a singleton, so
--   the stage will be created the first time this function is called
--   (typically, inside 'clutterInit'); all the subsequent calls to
--   'stageGetDefault' will return the same instance.
--   Clutter guarantess the existence of the default stage.
{# fun unsafe stage_get_default as ^ { } -> `Stage' newStage* #}

-- | Creates a new, non-default stage. A non-default stage is a new
--   top-level actor which can be used as another container. It works
--   exactly like the default stage, but while 'stageGetDefault' will
--   always return the same instance, you will have to keep any
--   'Stage' returned by 'stageNew'.
--
--   The ability to support multiple stages depends on the current
--   backend. Use 'clutterFeatureAvailable' and
--   CLUTTER_FEATURE_STAGE_MULTIPLE to check at runtime whether a
--   backend supports multiple stages.
--
--  [@Returns@] @Just@ a new stage, or @Nothing@ if the default
--   backend does not support multiple stages. Use 'actorDestroy' to
--   programmatically close the returned stage.
--
{# fun unsafe stage_new as ^ { } -> `Maybe Stage' maybeNewStage* #}

-- | Checks if stage is the default stage, or an instance created using
--  'stageNew' but internally using the same implementation.
--
{# fun unsafe stage_is_default as ^ `(StageClass stage)' =>
    { withStageClass* `stage' } -> `Bool' #}

-- | Sets the stage color.
{# fun unsafe stage_set_color as ^ `(StageClass stage)' =>
    { withStageClass* `stage', withColor* `Color' } -> `()' #}

-- | Retrieves the stage color.
{# fun unsafe stage_get_color as ^ `(StageClass stage)' =>
    { withStageClass* `stage', alloca- `Color' peek*} -> `()' #}


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
{# fun unsafe stage_set_fullscreen as ^ `(StageClass stage)' =>
    { withStageClass* `stage', `Bool'} -> `()' #}
-- | Retrieves whether the stage is full screen or not
{# fun unsafe stage_get_fullscreen as ^ `(StageClass stage)' =>
    { withStageClass* `stage' } -> `Bool' #}


-- | Shows the cursor on the stage window
{# fun unsafe stage_show_cursor as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `()' #}
-- | Makes the cursor invisible on the stage window
{# fun unsafe stage_hide_cursor as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `()' #}

--CHECKME: Should I even include non-application functions?

-- | Checks the scene at the coordinates x and y and returns a pointer to the 'Actor' at those coordinates.
--
--  By using pick_mode it is possible to control which actors will be painted and thus available.
{# fun unsafe stage_get_actor_at_pos as ^ `(StageClass stage)' =>
       { withStageClass* `stage', cFromEnum `PickMode', `Int', `Int'} -> `Actor' newActor* #}

-- | This function essentially makes sure the right GL context is
--   current for the passed stage. It is not intended to be used by
--   applications.
{# fun unsafe stage_ensure_current as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `()' #}

-- | Ensures that the GL viewport is updated with the current stage
--   window size.
--
-- This function will queue a redraw of stage.
--
-- This function should not be called by applications; it is used when
-- embedding a ClutterStage into a toolkit with another windowing
-- system, like GTK+.
{# fun unsafe stage_ensure_viewport as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `()' #}

-- | Ensures that stage is redrawn
--
-- This function should not be called by applications: it is used when
-- embedding a 'Stage' into a toolkit with another windowing
-- system, like GTK+.
{# fun unsafe stage_ensure_redraw as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `()' #}

-- | Queues a redraw for the passed stage.
--
-- Note
--
-- Applications should call 'actorQueueRedraw' and not this function.
-- Note
--
-- This function is just a wrapper for 'actorQueueRedraw' and should probably go away.
{# fun unsafe stage_queue_redraw as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `()' #}

--CHECKME this might fall under the category of low level event stuff we're not dealing with
--{# fun unsafe stage_event as ^ `(StageClass stage)' => { withStageClass* `stage', withEvent* `Event' } -> `Bool' #}


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
{# fun unsafe stage_set_key_focus as ^ `(StageClass stage, ActorClass actor)' =>
       { withStageClass* `stage', withMaybeActorClass* `Maybe actor' } -> `()' #}

-- | Retrieves the actor that is currently under key focus.
--
-- [@stage@] the 'Stage'
--
-- [@Returns@] the actor with key focus, or the stage.
--
-- * Since 0.6
--
{# fun unsafe stage_get_key_focus as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `Actor' newActor* #}

--TODO: Same problem as other places, setting and getting ActorClass is unhappy
--stageKeyFocus :: (StageClass stage, ActorClass actor) => Attr stage actor
--stageKeyFocus = newAttr stageGetKeyFocus stageSetKeyFocus

--Why is this scattered around in many places in gtk2hs?
foreign import ccall unsafe "&g_free"
  finalizerGFree :: FinalizerPtr a


stageReadPixels :: (StageClass stage) =>
                   stage
                   -> Int
                   -> Int
                   -> Int
                   -> Int
                   -> IO (Maybe (RGBData (Int, Int) Word8))
stageReadPixels stage x y w h = let cx = cIntConv x
                                    cy = cIntConv y
                                    cw = cIntConv w
                                    ch = cIntConv h
                                in withStageClass stage $ \stgPtr -> do
                                  sizeW <- if w == -1
                                              then liftM floor (actorGetWidth stage)
                                              else return w
                                  sizeH <- if h == -1
                                              then liftM floor (actorGetHeight stage)
                                              else return h
                                  ptr <- {# call unsafe stage_read_pixels #} stgPtr cx cy cw ch
                                  putStrLn $ "ReadW: " ++ Prelude.show sizeW ++ "  ReadH: " ++ Prelude.show sizeH
                                  if ptr == nullPtr
                                     then return Prelude.Nothing
                                     else do fptr <- newForeignPtr_  ptr
                                           --fptr <- newForeignPtr finalizerGFree ptr
                                             arr <- unsafeForeignPtrToStorableArray (castForeignPtr fptr) ((1, 1), (4*sizeW, sizeH)) :: IO (StorableArray (Int, Int) Word8)
                                             bounds <- getBounds arr
                                             let rs = rangeSize bounds
                                             --arst <- fmap length (getElems arr)
                                             putStrLn $ "Range size: " ++ (Prelude.show rs) -- 2,560,000
                                             --putStrLn $ "Length: " ++ (Prelude.show arst)
                                             let pos = (2*sizeW + 3, sizeH `div` 2 - 4)
                                             let arst = unsafeIndex ((1,1), (4*sizeW, sizeH)) pos
                                             let rowstride = sizeW * 4
                                             putStrLn $ "ARST: " ++ Prelude.show arst
                                             junk <- readArray arr pos
                                             allLen <- fmap length (getElems arr)
                                             non0 <- fmap (filter (/= 0)) (getElems arr)
                                             putStrLn $ "LENGTHALL: " ++ Prelude.show allLen
                                             putStrLn $ "LOL: " ++ Prelude.show non0
                                             putStrLn ("Not dead: " ++ Prelude.show junk)
                                             return $ Just (mkRGBData arr True rowstride)


-- | Sets whether motion events received between redraws should be
-- throttled or not. If motion events are throttled, those events
-- received by the windowing system between redraws will be compressed
-- so that only the last event will be propagated to the stage and its
-- actors.  This function should only be used if you want to have all
-- the motion events delivered to your application code.
{# fun unsafe stage_set_throttle_motion_events as ^ `(StageClass stage)' =>
    { withStageClass* `stage', `Bool' } -> `()' #}

-- | Retrieves the value set with 'stageSetThrottleMotionEvents'
{# fun unsafe stage_get_throttle_motion_events as ^ `(StageClass stage)' =>
    { withStageClass* `stage' } -> `Bool' #}


-- | Retrieves the stage perspective.
{# fun unsafe stage_get_perspective as ^ `(StageClass stage)' =>
    { withStageClass* `stage', alloca- `Perspective' peek* } -> `()' #}
-- | Sets the stage perspective.
{# fun unsafe stage_set_perspective as ^ `(StageClass stage)' =>
    { withStageClass* `stage', withPerspective* `Perspective'} -> `()' #}



--TODO: Unicode???
-- | Sets the stage title.
{# fun unsafe stage_set_title as ^ `(StageClass stage)' =>
    { withStageClass* `stage', withMaybeString* `Maybe String' } -> `()' #}
-- | Gets the stage title.
{# fun unsafe stage_get_title as ^ `(StageClass stage)' =>
    { withStageClass* `stage' } -> `Maybe String' maybeString* #}

-- | Sets if the stage is resizable by user interaction (e.g. via window manager controls)
{# fun unsafe stage_set_user_resizable as ^ `(StageClass stage)' =>
    { withStageClass* `stage', `Bool' } -> `()' #}
-- | Retrieves the value set with 'stageSetUserResizable'.
{# fun unsafe stage_get_user_resizable as ^ `(StageClass stage)' =>
    { withStageClass* `stage' } -> `Bool' #}


-- | Sets whether the depth cueing effect on the stage should be enabled or not.
--
--   Depth cueing is a 3D effect that makes actors farther away from
--   the viewing point less opaque, by fading them with the stage
--   color.
--
-- The parameters of the GL fog used can be changed using the 'stageSetFog' function.
{# fun unsafe stage_set_use_fog as ^ `(StageClass stage)' =>
    { withStageClass* `stage', `Bool' } -> `()' #}

-- | Gets whether the depth cueing effect is enabled on stage.
{# fun unsafe stage_get_use_fog as ^ `(StageClass stage)' => { withStageClass* `stage' } -> `Bool' #}


--TODO: I may change Cogl to not have colorNew usage for example
-- | Sets the fog \(also known as "depth cueing"\) settings for the stage.
--
--  A 'Stage' will only use a linear fog progression, which depends
--  solely on the distance from the viewer. The 'Cogl.setFog' function
--  in COGL exposes more of the underlying implementation, and allows
--  changing the for progression function. It can be directly used by
--  disabling the 'stageUseFog' property and connecting a signal
--  handler to the 'paint' signal on the stage. The paint signal
--  handler will call Cogl.setFog with the desired settings, like:
--
-- @
--
--  do
--    stageSetUseFog stage True
--    stage `on` paint $ do fogColor <- Cogl.colorNew
--                        \(Color r g b a\) \<- stageGetColor stage
--                   \-\- set the fog color from the stage background color
--                        fogColor \<\- Cogl.colorSetFromFrom4ub fogColor r g b a
--                   \-\- enable fog
--                        Cogl.setFog fogColor
--                                    Cogl.FogModeModeExponential -- mode
--                                    0.5       -- Density
--                                    5.0 30.0  -- z_near and z_far
-- @
--
--  Note: The fogging functions only work correctly when the visible
--  actors use unmultiplied alpha colors. By default Cogl will
--  premultiply textures and 'Cogl.setSourceColor' will premultiply
--  colors, so unless you explicitly load your textures requesting an
--  unmultiplied internal_format and use Cogl.materialSetColor you
--  can only use fogging with fully opaque actors.
--
-- We can look to improve this in the future when we can depend on
--  fragment shaders.
{# fun unsafe stage_set_fog as ^ `(StageClass stage)' =>
    { withStageClass* `stage', withFog* `Fog' } -> `()' #}

-- | Retrieves the current depth cueing settings from the stage.
{# fun unsafe stage_get_fog as ^ `(StageClass stage)' =>
    { withStageClass* `stage', alloca- `Fog' peek* } -> `()' #}


-- Attributes


-- | The color of the main stage.
stageColor :: (StageClass stage) => Attr stage Color
stageColor = newNamedAttr "color" stageGetColor stageSetColor


-- | Whether the mouse pointer should be visible
--
-- Default value: @True@
--
stageCursorVisible :: (StageClass stage) => Attr stage Bool
stageCursorVisible = newAttrFromBoolProperty "cursor-visible"


-- | Whether the main stage is fullscreen.
--
-- Default value: @False@
--
stageFullscreenSet :: (StageClass stage) => ReadAttr stage Bool
stageFullscreenSet = readAttrFromBoolProperty "fullscreen-set"

-- | Whether the stage should be rendered in an offscreen buffer.
--
-- * Warning
--
-- Not every backend supports redirecting the stage to an offscreen
-- buffer. This property might not work and it might be deprecated at
-- any later date.
--
-- Default value: @False@
--
stageOffscreen :: (StageClass stage) => Attr stage Bool
stageOffscreen = newAttrFromBoolProperty "offscreen"


-- | The parameters used for the perspective projection from 3D
--   coordinates to 2D
--
-- * Since 0.8.2
--
stagePerspective :: (StageClass stage) => Attr stage Perspective
stagePerspective = newNamedAttr "perspective" stageGetPerspective stageSetPerspective


-- | The stage's title - usually displayed in stage windows title
--   decorations.
--
-- Default value: @Nothing@
--
-- * Since 0.4
--
stageTitle :: (StageClass stage) => Attr stage (Maybe String)
stageTitle = newNamedAttr "title" stageGetTitle stageSetTitle


-- | Whether the stage is resizable via user interaction.
--
-- Default value: @False@
--
-- * Since 0.4
--
stageUserResizable :: (StageClass stage) => Attr stage Bool
stageUserResizable = newNamedAttr "user-resizable" stageGetUserResizable stageSetUserResizable


-- | Whether the stage should use a linear GL "fog" in creating the
--   depth-cueing effect, to enhance the perception of depth by fading
--   actors farther from the viewpoint.
--
-- Default value: @False@
--
-- * Since 0.6
--
stageUseFog :: (StageClass stage) => Attr stage Bool
stageUseFog = newNamedAttr "use-fog" stageGetUseFog stageSetUseFog


-- | The settings for the GL "fog", used only if "use-fog" is set to
--   @True@
--
-- * Since 1.0
--
stageFog :: (StageClass stage) => Attr stage Fog
stageFog = newNamedAttr "fog" stageGetFog stageSetFog


-- Signals

--See note in Types of Activatable
-- | The 'activate' signal is emitted when the stage receives key focus from the underlying window system.
instance Activatable Stage where
  activate = Signal (connect_NONE__NONE "activate")


-- | The 'deactivate' signal is emitted when the stage loses key focus
--   from the underlying window system.
deactivate :: (StageClass stage) => Signal stage (IO ())
deactivate = Signal (connect_NONE__NONE "deactivate")


-- | The 'fullscreen' signal is emitted when the stage is made fullscreen.
fullscreen :: (StageClass stage) => Signal stage (IO ())
fullscreen = Signal (connect_NONE__NONE "fullscreen")

-- | The 'unfullscreen' signal is emitted when the stage leaves a fullscreen state.
unfullscreen :: (StageClass stage) => Signal stage (IO ())
unfullscreen = Signal (connect_NONE__NONE "unfullscreen")


