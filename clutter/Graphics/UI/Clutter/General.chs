-- -*-haskell-*-
--  Clutter General
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>
#include <pango/pango.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.General (

-- * Types
  InitError(..),
  FontFlags(..),

-- * General functions
  clutterInit,
  clutterMain,
  mainQuit,
  mainLevel,

  getDebugEnabled,
  getShowFps,
  getTimestamp,

  getActorByGid,

  setDefaultFrameRate,
  getDefaultFrameRate,
  setMotionEventsEnabled,
  getMotionEventsEnabled,
  clearGlyphCache,
  setFontFlags,
  getFontFlags,
  getFontMap,

  getKeyboardGrab,
  getPointerGrab,
  grabKeyboard,
  grabPointer,
  ungrabKeyboard,
  ungrabPointer,
  grabPointerForDevice,
  ungrabPointerForDevice,
--doEvent
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.GObject
import Graphics.UI.Gtk.Pango.Types
import Graphics.UI.Gtk.Types (FontMap, mkFontMap)
import System.Glib.UTFString
import System.Environment (getProgName, getArgs)
import Control.Monad (liftM, when)


-- | Starts the Clutter mainloop.
{# fun main as clutterMain { } -> `()' #}

-- | Terminates the Clutter mainloop.
{# fun main_quit as ^ { } -> `()' #}

-- | Retrieves the depth of the Clutter mainloop.
--
-- [@Returns@] The level of the mainloop.
--
{# fun main_level as ^ { } -> `Int' #}


--just make it go for now. I think some issues with it
--out args? do they matter? also how to use out marshallers correctly
clutterInit :: IO InitError
clutterInit = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen addrs $ \argc argv ->
    with argv $ \argvp ->
    with argc $ \argcp -> do
                res <- liftM cToEnum $ {# call unsafe init #} (castPtr argcp) (castPtr argvp)
                when (res /= InitSuccess) $ error ("Cannot initialize Clutter." ++ show res)
                return res


-- | Check if clutter has debugging turned on.
--
-- [@Returns@] @True@ if debugging is turned on, @False@ otherwise.
--
{# fun unsafe get_debug_enabled as ^ { } -> `Bool' #}

-- | Returns whether Clutter should print out the frames per second on
--   the console. You can enable this setting either using the
--   CLUTTER_SHOW_FPS environment variable or passing the
--   --clutter-show-fps command line argument. *
--
-- [@Returns@] @True@ if Clutter should show the FPS.
--
-- * Since 0.4
--
{# fun unsafe get_show_fps as ^ { } -> `Bool' #}

-- | Returns the approximate number of microseconds passed since
--   clutter was intialised.
--
-- [@Returns@] Number of microseconds since 'clutterInit' was called.
--
{# fun unsafe get_timestamp as ^ { } -> `Timestamp' cIntConv #}

-- | Retrieves the ClutterActor with id.
--
-- [@id@] a ClutterActor ID.
--
-- [@Returns@] @Just@ an Actor or @Nothing@
--
-- * Since 0.6
--
{# fun unsafe get_actor_by_gid as ^ { cIntConv `GID' } -> `Maybe Actor' maybeNewActor* #}

-- | Sets the default frame rate. This frame rate will be used to
--   limit the number of frames drawn if Clutter is not able to
--   synchronize with the vertical refresh rate of the display. When
--   synchronization is possible, this value is ignored.
--
-- [@frames_per_sec@] the new default frame rate
--
-- * Since 0.6
--
{# fun unsafe set_default_frame_rate as ^ { cIntConv `Word' } -> `()' #}

-- | Retrieves the default frame rate. See 'setDefaultFrameRate'.
--
-- [@Returns@] the default frame rate
--
-- * Since 0.6
--
{# fun unsafe get_default_frame_rate as ^ { } -> `Word' cIntConv #}

-- | Sets whether per-actor motion events should be enabled or not
--   (the default is to enable them).
--
-- If enable is @False@ the following events will not work:
--
-- 'Actor'::'motionEvent', unless on the 'Stage'
--
-- 'Actor'::'enterEvent'
--
-- 'Actor'::'leaveEvent'
--
-- [@enable@] @True@ to enable per-actor motion events
--
-- * Since 0.6
--
{# fun unsafe set_motion_events_enabled as ^ { `Bool' } -> `()' #}


-- | Gets whether the per-actor motion events are enabled.
--
-- [@Returns@] @True@ if the motion events are enabled
--
-- * Since 0.6
--
{# fun unsafe get_motion_events_enabled as ^ { } -> `Bool' #}

-- | Clears the internal cache of glyphs used by the Pango
--   renderer. This will free up some memory and GL texture
--   resources. The cache will be automatically refilled as more text
--   is drawn.
--
-- * Since 0.8
--
{# fun unsafe clear_glyph_cache as ^ { } -> `()' #}

-- | Sets the font quality options for subsequent text rendering
--   operations.
--
-- Using mipmapped textures will improve the quality for scaled down
-- text but will use more texture memory.
--
-- Enabling hinting improves text quality for static text but may
-- introduce some artifacts if the text is animated.
--
-- [@flags@] The new flags
--
-- * Since 1.0
--
{# fun unsafe set_font_flags as ^ { cFromFlags `[FontFlags]' } -> `()' #}

-- | Gets the current font flags for rendering text. See 'setFontFlags'.
--
-- [@Returns@] The font flags
--
-- * Since 1.0
--
{# fun unsafe get_font_flags as ^ { } -> `[FontFlags]' cToFlags #}

{# pointer *PangoFontMap as FontMapPtr foreign -> FontMap nocode #}

-- | Retrieves the PangoFontMap instance used by Clutter. You can use
--   the global font map object with the COGL Pango API.
--
-- * Since 1.0
--
getFontMap :: IO FontMap
getFontMap = makeNewGObject mkFontMap $ {# call unsafe get_font_map #}

--TODO: What about threads and timeouts etc.



-- | Queries the current keyboard grab of clutter.
--
-- [@Returns@] @Just@ the actor currently holding the keyboard grab,
--              or @Nothing@ if there is no grab.. transfer none.
--
-- * Since 0.6
--
{# fun unsafe get_keyboard_grab as ^ { } -> `Maybe Actor' maybeNewActor* #}


-- | Queries the current pointer grab of clutter.
--
-- [@Returns@] @Just@ the actor currently holding the pointer grab, or
-- @Nothing@ if there is no grab.
--
-- * Since 0.6
--
{# fun unsafe get_pointer_grab as ^ { } -> `Maybe Actor' maybeNewActor* #}


-- | Grabs keyboard events, after the grab is done keyboard events
--   ('keyPressEvent' and 'keyReleaseEvent') are delivered to this
--   actor directly. The source set in the event will be the actor
--   that would have received the event if the keyboard grab was not
--   in effect.
--
-- Like pointer grabs, keyboard grabs should only be used as a last
-- resource.
--
-- See also 'stageSetKeyFocus' and 'actorGrabKeyFocus' to perform a
-- \"soft\" key grab and assign key focus to a specific actor.
--
-- [@actor@] an Actor
--
-- * Since 0.6
--
{# fun unsafe grab_keyboard as ^ { withActor* `Actor' } -> `()' #}




-- | Grabs pointer events, after the grab is done all pointer related
--   events (press, motion, release, enter, leave and scroll) are
--   delivered to this actor directly without passing through both
--   capture and bubble phases of the event delivery chain. The source
--   set in the event will be the actor that would have received the
--   event if the pointer grab was not in effect.
--
-- * Note
--
-- Grabs completely override the entire event delivery chain done by
-- Clutter. Pointer grabs should only be used as a last resource;
-- using the 'capturedEvent' signal should always be the preferred way
-- to intercept event delivery to reactive actors.
--
-- If you wish to grab all the pointer events for a specific input
-- device, you should use 'grabPointerForDevice'.
--
-- [@actor@] an Actor
--
-- * Since 0.6
--
{# fun unsafe grab_pointer as ^ { withActor* `Actor' } -> `()' #}

-- | Removes an existing grab of the keyboard.
--
-- * Since 0.6
--
{# fun unsafe ungrab_keyboard as ^ { } -> `()' #}


-- | Removes an existing grab of the pointer.
--
-- * Since 0.6
--
{# fun unsafe ungrab_pointer as ^ { } -> `()' #}

-- | Grabs all the pointer events coming from the device id for actor.
--
--  If id is -1 then this function is equivalent to 'grabPointer'.
--
-- [@actor@] an Actor
--
-- [@id@] a device id, or -1
--
-- * Since 0.8
--
{# fun unsafe grab_pointer_for_device as ^ { withActor* `Actor', `Int' } -> `()' #}


-- | Removes an existing grab of the pointer events for device id.
--
-- [@id@] a device id
--
-- * Since 0.8
--
{# fun unsafe ungrab_pointer_for_device as ^ { `Int' } -> `()' #}

--{# fun unsafe clutter_do_event { withEvent* `Event' } -> `()' #}

