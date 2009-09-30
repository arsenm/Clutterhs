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
                                      rectangleNew,
                                      rectangleNewWithColor,
                                      rectangleGetColor,
                                      rectangleSetColor,
                                      rectangleColor,

                                      rectangleGetBorderWidth,
                                      rectangleSetBorderWidth,
                                      rectangleBorderWidth

                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties

{# fun unsafe rectangle_new as ^ {} -> `Rectangle' newRectangle* #}
{# fun unsafe rectangle_new_with_color as ^ {withColor* `Color'} -> `Rectangle' newRectangle* #}

{# fun unsafe rectangle_get_color as ^ {withRectangle* `Rectangle', alloca- `Color' peek* } -> `()' #}
{# fun unsafe rectangle_set_color as ^ {withRectangle* `Rectangle', withColor* `Color'} -> `()' #}
rectangleColor :: Attr Rectangle Color
rectangleColor = newAttr rectangleGetColor rectangleSetColor

--FIXME: guint vs. int
{# fun unsafe rectangle_get_border_width as ^ {withRectangle* `Rectangle'} -> `Int' #}
{# fun unsafe rectangle_set_border_width as ^ {withRectangle* `Rectangle', `Int'} -> `()' #}
rectangleBorderWidth :: Attr Rectangle Int
rectangleBorderWidth = newAttr rectangleGetBorderWidth rectangleSetBorderWidth

