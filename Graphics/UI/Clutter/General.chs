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

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.General (
                                    clutterInit,
                                    clutterMain,
                                    clutterMainQuit,


                                    getKeyboardGrab,
                                    getPointerGrab,
                                    grabKeyboard,
                                    ungrabKeyboard,
                                    grabPointerForDevice,
                                    ungrabPointerForDevice
                                  --doEvent
                                   ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.GObject
import System.Glib.UTFString
import System.Environment (getProgName, getArgs)
import Control.Monad (liftM, mapM, when)

{# fun main as clutterMain { } -> `()' #}
{# fun main_quit as clutterMainQuit { } -> `()' #}

--FIXME: use of id as marshaller seems horribly wrong
{# fun unsafe clutter_init as secretClutterInit {id `Ptr CInt', id `Ptr (Ptr (CString))'} -> `InitError' cToEnum #}

--just make it go for now. I think some issues with it
--out args? do they matter? also how to use out marshallers correctly
clutterInit :: IO InitError
clutterInit = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen addrs   $ \argc argv ->
    with argv $ \argvp ->
    with argc $ \argcp -> do
--              res <- {#call unsafe init#} (castPtr argcp) (castPtr argvp)
                res <- secretClutterInit (castPtr argcp) (castPtr argvp)
                when (res /= InitSuccess) $ error ("Cannot initialize Clutter." ++ show res)
                return res



{# fun unsafe get_debug_enabled as ^ { } -> `Bool' #}
{# fun unsafe get_show_fps as ^ { } -> `Bool' #}

--CHECKME: GULong
{# fun unsafe get_timestamp as ^ { } -> `Word' cIntConv #}

--TODO: GID type
{# fun unsafe get_actor_by_gid as ^ { `Word32' } -> `Actor' newActor* #}

{# fun unsafe set_default_frame_rate as ^ { cIntConv `Word' } -> `()' #}
{# fun unsafe get_default_frame_rate as ^ { } -> `Word' cIntConv #}

{# fun unsafe set_motion_events_enabled as ^ { `Bool' } -> `()' #}
{# fun unsafe get_motion_events_enabled as ^ { } -> `Bool' #}
{# fun unsafe clear_glyph_cache as ^ { } -> `()' #}
{# fun unsafe set_font_flags as ^ { cFromFlags `[FontFlags]' } -> `()' #}
{# fun unsafe get_font_flags as ^ { } -> `[FontFlags]' cToFlags #}


--TODO: A bunch of functions here
--{# fun unsafe get_font_map as ^ { } -> `FontMap' #}
--{# threads_set_lock_functions




{# fun unsafe get_keyboard_grab as ^ { } -> `Actor' newActor* #}
{# fun unsafe get_pointer_grab as ^ { } -> `Actor' newActor* #}
{# fun unsafe grab_keyboard as ^ { withActor* `Actor' } -> `()' #}
{# fun unsafe grab_pointer as ^ { withActor* `Actor' } -> `()' #}
{# fun unsafe ungrab_keyboard as ^ { } -> `()' #}
{# fun unsafe ungrab_pointer as ^ { } -> `()' #}
{# fun unsafe grab_pointer_for_device as ^ { withActor* `Actor', `Int' } -> `()' #}
{# fun unsafe ungrab_pointer_for_device as ^ { `Int' } -> `()' #}

--{# fun unsafe clutter_do_event { withEvent* `Event' } -> `()' #}

