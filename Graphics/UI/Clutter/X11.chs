-- -*-haskell-*-
--  Clutter X11 Specific stuff
--
--  Author : Matthew Arsenault
--
--  Created: 25 Oct 2009
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
#include <clutter/x11/clutter-x11.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.X11 (
                              --x11SetDisplay,
                                x11DisableEventRetrieval,
                                x11HasEventRetrieval,
                              --x11GetStageFromWindow,
                              --x11HandleEvent,
                              --x11GetDefaultDisplay,
                              --x11GetDefaultScreen,
                              --x11GetRootWindow,
                              --x11GetStageVisual,
                              --x11GetStageWindow,
                              --x11GetStageForeign,
                              --x11SetStageForeign,
                              --x11StageForeign,
                                x11TrapXErrors,
                                x11UntrapXErrors,
                                x11HasCompositeExtension,
                              --x11GetCurrentEventTime,
                              --x11AddFilter,
                              --x11RemoveFilter,
                              --x11GetInputDevices,
                                x11HasXinput,
                                x11EnableXinput,
                              --x11TexturePixmapNew,
                              --x11TexturePixmapNewWithPixmap,
                              --x11TexturePixmapNewWithWindow,
                              --xllTexturePixmapSetPixmap,
                              --xllTexturePixmapSetWindow,
                              --x11TexturePixmapSyncWindow,
                              --x11TexturePixmapUpdateArea,
                              --x11TexturePixmapSetAutomatic,
                                X11FilterReturn(..),
                              --X11FilterFunc,
                                X11XInputEventTypes(..)
                              --X11TexturePixmap,
                              --X11TexturePixmapClass
                               ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject
import System.Glib.UTFString
import System.Environment (getProgName, getArgs)
import Control.Monad (liftM, mapM, when)

{# enum ClutterX11FilterReturn as X11FilterReturn {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterX11XInputEventTypes as X11XInputEventTypes {underscoreToCase} deriving (Show, Eq) #}

{# fun unsafe x11_disable_event_retrieval as ^ { } -> `()' #}
{# fun unsafe x11_has_event_retrieval as ^ { } -> `Bool' #}

{# fun unsafe x11_trap_x_errors as ^ { } -> `()' #}
{# fun unsafe x11_untrap_x_errors as ^ { } -> `()' #}
{# fun unsafe x11_has_composite_extension as ^ { } -> `Bool' #}
--{# fun unsafe x11_get_current_event_time as ^ { } -> `Time' #}

{# fun unsafe x11_has_xinput as ^ { } -> `Bool' #}
{# fun unsafe x11_enable_xinput as ^ { } -> `()' #}


