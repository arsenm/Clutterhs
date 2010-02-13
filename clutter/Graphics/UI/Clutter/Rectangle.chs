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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- | 'Rectangle' — An actor that displays a simple rectangle.
module Graphics.UI.Clutter.Rectangle (
-- * Description
-- | 'Rectangle' is a ClutterActor which draws a simple filled
-- rectangle.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'Rectangle'
-- @

-- * Types
  Rectangle,
  RectangleClass,

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
  rectangleBorderWidth,
  rectangleHasBorder
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Properties
import System.Glib.Attributes


-- | Creates a new actor with a rectangular shape.
{# fun unsafe rectangle_new as ^ { } -> `Rectangle' newRectangle* #}

-- | Creates a new actor with a rectangular shape and of the given /color/
{# fun unsafe rectangle_new_with_color as ^ { withColor* `Color'} -> `Rectangle' newRectangle* #}

-- | Retrieves the color of rectangle
{# fun unsafe rectangle_get_color as ^ { withRectangle* `Rectangle', alloca- `Color' peek* } -> `()' #}
-- | Sets the color of a rectangle
{# fun unsafe rectangle_set_color as ^ { withRectangle* `Rectangle', withColor* `Color' } -> `()' #}

-- | Gets the color of the border used by rectangle
--
-- [@rectangle@] a 'Rectangle'
--
-- [@color@] The color of the rectangle's border
--
-- * Since 0.2
---
{# fun unsafe rectangle_get_border_color as ^ { withRectangle* `Rectangle', alloca- `Color' peek* } -> `()' #}


-- | Sets the color of the border used by rectangle using color
--
-- [@rectangle@] a 'Rectangle'
--
-- [@color@] the color of the border
--
{# fun unsafe rectangle_set_border_color as ^ { withRectangle* `Rectangle', withColor* `Color'} -> `()' #}

-- | Gets the width (in pixels) of the border used by rectangle
--
-- [@rectangle@] a 'Rectangle'
--
-- [@Returns@] the border's width
--
-- * Since 0.2
--
{# fun unsafe rectangle_get_border_width as ^ { withRectangle* `Rectangle' } -> `Word' cIntConv #}

-- | Sets the width (in pixel) of the border used by rectangle. A width of 0 will unset the border
--
-- [@rectangle@] a 'Rectangle'
--
-- [@width@] the width of the border
--
-- * Since 0.2
--
{# fun unsafe rectangle_set_border_width as ^ { withRectangle* `Rectangle', cIntConv `Word' } -> `()' #}

-- | The color of the rectangle.
--
-- * Since 0.2
--
rectangleColor :: Attr Rectangle Color
rectangleColor = newNamedAttr "color" rectangleGetColor rectangleSetColor

-- | The color of the border of the rectangle.
rectangleBorderColor :: Attr Rectangle Color
rectangleBorderColor = newNamedAttr "border-color" rectangleGetBorderColor rectangleSetBorderColor

-- | The width of the border of the rectangle, in pixels.
--
-- Default value: 0
--
-- * Since 0.2
--
rectangleBorderWidth :: Attr Rectangle Word
rectangleBorderWidth = newNamedAttr "border-width" rectangleGetBorderWidth rectangleSetBorderWidth


-- | Whether the 'Rectangle' should be displayed with a border.
--
-- Default value: @False@
--
-- * Since 0.2
--
rectangleHasBorder :: Attr Rectangle Bool
rectangleHasBorder = newAttrFromBoolProperty "has-border"

