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
                                   gTypeColor,
                                   gTypeVertex,
                                   gTypeGravity,
                                   gTypeRequestMode,
                                   gTypeGeometry,
                                   gTypeActor,
                                   gTypeAlpha
                                  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GValue
import System.Glib.GType

--TODO: Move these
{# fun pure unsafe color_get_type as gTypeColor { } -> `GType' cToEnum #}
{# fun pure unsafe vertex_get_type as gTypeVertex { } -> `GType' cToEnum #}
{# fun pure unsafe gravity_get_type as gTypeGravity { } -> `GType' cToEnum #}
{# fun pure unsafe request_mode_get_type as gTypeRequestMode { } -> `GType' cToEnum #}
{# fun pure unsafe geometry_get_type as gTypeGeometry { } -> `GType' cToEnum #}
{# fun pure unsafe actor_get_type as gTypeActor { } -> `GType' cToEnum #}
{# fun pure unsafe alpha_get_type as gTypeAlpha { } -> `GType' cToEnum #}

