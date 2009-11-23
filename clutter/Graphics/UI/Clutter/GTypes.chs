-- -*-haskell-*-
--  GTypes for Clutter
--
--  Author : Matthew Arsenault
--
--  Created: 21 Nov 2009
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

module Graphics.UI.Clutter.GTypes (
                                   color,
                                   vertex,
                                   gravity,
                                   requestmode,
                                   geometry,
                                   actor,
                                   alpha
                                  ) where

import C2HS
import System.Glib.GType

--TODO: Move these
{# fun pure unsafe color_get_type as color { } -> `GType' cToEnum #}
{# fun pure unsafe vertex_get_type as vertex { } -> `GType' cToEnum #}
{# fun pure unsafe gravity_get_type as gravity { } -> `GType' cToEnum #}
{# fun pure unsafe request_mode_get_type as requestmode { } -> `GType' cToEnum #}
{# fun pure unsafe geometry_get_type as geometry { } -> `GType' cToEnum #}
{# fun pure unsafe actor_get_type as actor { } -> `GType' cToEnum #}
{# fun pure unsafe alpha_get_type as alpha { } -> `GType' cToEnum #}

