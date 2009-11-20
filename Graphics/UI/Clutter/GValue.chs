-- -*-haskell-*-
--  GValue
--
--  Author : Matthew Arsenault
--
--  Created: 25 Sep 2009
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
             TypeSynonymInstances,
             FlexibleInstances,
             UndecidableInstances #-}


#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

--TODO: Lots of stuff here should be private?
--FIXME: Messy names
--TODO: Merge this with StoreValue, since this is what you actually want

module Graphics.UI.Clutter.GValue (
                                   gTypeColor,
                                   gTypeVertex,
                                   gTypeGravity,
                                   gTypeRequestMode,
                                   gTypeGeometry,
                                   gTypeActor,
                                   valueSetColor,
                                   valueGetColor
                                  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GValue
import System.Glib.GType


--Color GValue
{# fun unsafe value_get_color as ^ { withGValue `GValue' } -> `Color' peek* #}
{# fun unsafe value_set_color as ^ { withGValue `GValue', withColor* `Color' } -> `()' #}

--TODO: Move these
--constant
{# fun pure unsafe color_get_type as gTypeColor { } -> `GType' cToEnum #}
{# fun pure unsafe vertex_get_type as gTypeVertex { } -> `GType' cToEnum #}
{# fun pure unsafe gravity_get_type as gTypeGravity { } -> `GType' cToEnum #}
{# fun pure unsafe request_mode_get_type as gTypeRequestMode { } -> `GType' cToEnum #}
{# fun pure unsafe geometry_get_type as gTypeGeometry { } -> `GType' cToEnum #}
{# fun pure unsafe actor_get_type as gTypeActor { } -> `GType' cToEnum #}

