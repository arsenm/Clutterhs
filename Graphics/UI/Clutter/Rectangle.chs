-- -*-haskell-*-
--  Clutter Rectangle
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Rectangle (
                                      rectangleGetColor

                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject

--instance Storable
--conv::RectangleClass o => o -> Ptr Rectangle -> IO b
--conv x = (withRectangle . toRectangle) x

--TODO: Check marshallers and alloc stuff for the color out arg
--{# fun unsafe rectangle_get_color as ^ {conv* `Rectangle', alloca- `Color' } -> `()' #}
rectangleGetColor = undefined


