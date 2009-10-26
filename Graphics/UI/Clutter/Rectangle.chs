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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Rectangle (
                                     -- * Class Hierarchy
                                     -- |
                                     -- @
                                     -- |  'GObject'
                                     -- |   +----'GInitiallyUnowned'
                                     -- |         +----'Actor'
                                     -- |               +----'Rectangle'
                                     -- @

                                      -- * Constructors
                                      rectangleNew,
                                      rectangleNewWithColor,

                                      -- * Methods
                                      rectangleGetColor,
                                      rectangleSetColor,

                                      rectangleGetBorderColor,
                                      rectangleSetBorderColor,
                                      rectangleGetBorderWidth,
                                      rectangleSetBorderWidth,

                                      -- * Attributes
                                      rectangleColor,
                                      rectangleBorderColor,
                                      rectangleBorderWidth
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

-- | Creates a new ClutterActor with a rectangular shape.
{# fun unsafe rectangle_new as ^ { } -> `Rectangle' newRectangle* #}

-- | Creates a new ClutterActor with a rectangular shape and of the given /color/
--
{# fun unsafe rectangle_new_with_color as ^ { withColor* `Color' } -> `Rectangle' newRectangle* #}

-- | Retrieves the color of rectangle
{# fun unsafe rectangle_get_color as ^ { withRectangle* `Rectangle', alloca- `Color' peek* } -> `()' #}
-- | Sets the color of a rectangle
{# fun unsafe rectangle_set_color as ^ { withRectangle* `Rectangle', withColor* `Color' } -> `()' #}

-- | The color of the rectangle.
rectangleColor :: Attr Rectangle Color
rectangleColor = newAttr rectangleGetColor rectangleSetColor

-- | Gets the color of the border used by rectangle
{# fun unsafe rectangle_get_border_color as ^ { withRectangle* `Rectangle', alloca- `Color' peek* } -> `()' #}
-- | Sets the color of the border used by rectangle using color
{# fun unsafe rectangle_set_border_color as ^ { withRectangle* `Rectangle', withColor* `Color'} -> `()' #}

-- | The color of the border of the rectangle.
rectangleBorderColor :: Attr Rectangle Color
rectangleBorderColor = newAttr rectangleGetBorderColor rectangleSetBorderColor

-- | Gets the width (in pixels) of the border used by rectangle
{# fun unsafe rectangle_get_border_width as ^ { withRectangle* `Rectangle' } -> `Word' cIntConv #}

-- | Sets the width (in pixel) of the border used by rectangle. A width of 0 will unset the border
{# fun unsafe rectangle_set_border_width as ^ { withRectangle* `Rectangle', cIntConv `Word' } -> `()' #}

-- | The width of the border of the rectangle, in pixels.
rectangleBorderWidth :: Attr Rectangle Word
rectangleBorderWidth = newAttr rectangleGetBorderWidth rectangleSetBorderWidth

