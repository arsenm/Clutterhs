-- -*-haskell-*-
--  COGL Types
--
--  Author : Matthew Arsenault
--
--  Created: 13 Dec 2009
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

#include <cogl/cogl.h>

{# context lib="cogl" prefix="cogl" #}

-- | Color Type
module Graphics.Cogl.Color (
-- * Types
  Color,

-- * Methods
  colorSetFrom4f,
  colorSetFrom4ub,
  colorCopy

) where

import C2HS
import Control.Monad (liftM)

{# import Graphics.Cogl.Types #}

{# fun unsafe color_new as ^ { } -> `Color' newColor* #}
{# fun unsafe color_copy as ^ { withColor* `Color' } -> `Color' newColor* #}



-- | Sets the values of the passed channels into a 'Cogl.Color'.
--
-- [@dest@] a 'Cogl.Color'
--
-- [@red@] value of the red channel, between 0 and 255
--
-- [@green@] value of the green channel, between 0 and 255
--
-- [@blue@] value of the blue channel, between 0 and 255
--
-- [@alpha@] value of the alpha channel, between 0 and 255
--
-- * Since 1.0
--
{# fun unsafe color_set_from_4ub as ^ { withColor* `Color',
                                        `Word8',
                                        `Word8',
                                        `Word8',
                                        `Word8' } -> `()' #}


-- | Sets the values of the passed channels into a 'Cogl.Color'
--
-- [@dest@] a 'Cogl.Color'
--
-- [@red@] value of the red channel, between 0 and 1.0
--
-- [@green@] value of the green channel, between 0 and 1.0
--
-- [@blue@] value of the blue channel, between 0 and 1.0
--
-- [@alpha@] value of the alpha channel, between 0 and 1.0
--
-- * Since 1.0
--
{# fun unsafe color_set_from_4f as ^ { withColor* `Color',
                                       `Float',
                                       `Float',
                                       `Float',
                                       `Float' } -> `()' #}

-- | Retrieves the red channel of color as a fixed point value between
-- 0 and 1.0.
--
-- [@color@] a Color
--
-- [@Returns@] the red channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_red as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the green channel of color as a fixed point value
-- between 0 and 1.0.
--
-- [@color@] a Color
--
-- [@Returns@] the green channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_green as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the blue channel of color as a fixed point value
-- between 0 and 1.0.
--
-- [@color@] a CoglColor
--
-- [@Returns@] the blue channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_blue as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the alpha channel of color as a fixed point value
-- between 0 and 1.0.
--
-- [@color@] a CoglColor
--
-- [@Returns@] the alpha channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_alpha as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the red channel of color as a byte value between 0 and
-- 255
--
-- [@color@] a CoglColor
--
-- [@Returns@] the red channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_red_byte as ^ { withColor* `Color' } -> `Word8' #}


-- | Retrieves the green channel of color as a byte value between 0
-- and 255
--
-- [@color@] a CoglColor
--
-- [@Returns@] the green channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_green_byte as ^ { withColor* `Color' } -> `Word8' #}


-- | Retrieves the blue channel of color as a byte value between 0 and
-- 255
--
-- [@color@] a CoglColor
--
-- [@Returns@] the blue channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_blue_byte as ^ { withColor* `Color' } -> `Word8' #}


-- |Retrieves the alpha channel of color as a byte value between 0 and
-- 255
--
-- [@color@] a CoglColor
--
-- [@Returns@] the alpha channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_alpha_byte as ^ { withColor* `Color' } -> `Word8' #}


-- | Retrieves the red channel of color as a floating point value
-- between 0.0 and 1.0
--
-- [@color@] a CoglColor
--
-- [@Returns@] the red channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_red_float as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the green channel of color as a floating point value
-- between 0.0 and 1.0
--
-- [@color@] a CoglColor
--
-- [@Returns@] the green channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_green_float as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the blue channel of color as a floating point value
-- between 0.0 and 1.0
--
-- [@color@] a CoglColor
--
-- [@Returns@] the blue channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_blue_float as ^ { withColor* `Color' } -> `Float' #}


-- | Retrieves the alpha channel of color as a floating point value
-- between 0.0 and 1.0
--
-- [@color@] a CoglColor
--
-- [@Returns@] the alpha channel of the passed color
--
-- * Since 1.0
--
{# fun unsafe color_get_alpha_float as ^ { withColor* `Color' } -> `Float' #}

