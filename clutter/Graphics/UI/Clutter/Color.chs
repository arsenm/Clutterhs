-- -*-haskell-*-
--  Clutter Color
--
--  Author : Matthew Arsenault
--
--  Created: 4 Sep 2009
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

-- | Colors — Color management and manipulation.
module Graphics.UI.Clutter.Color (
-- * Description
-- | 'Color' is a simple type for representing colors in Clutter.
--
-- A 'Color is expressed as a group of 4 alues ranging from zero to
-- 255, one for each color channel plus one for the alpha.
--
  colorFromString,
  colorToString,

  colorFromHls,
  colorToHls,
  colorFromPixel,
  colorToPixel,
  colorAdd,
  colorSubtract,
  colorLighten,
  colorDarken,
  colorShade
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Data.Word



-- | Parses a string definition of a color, filling the red, green,
--   blue and alpha channels of color. If alpha is not specified it
--   will be set full opaque.
--
-- The color may be defined by any of the formats understood by
-- Pango.colorFromString; these include literal color names, like Red
-- or DarkSlateGray, or hexadecimal specifications like #3050b2 or
-- #333.
--
-- * Since 1.0
--
colorFromString :: String       -- ^ a string specifiying a color (named color or RRGGBBAA)
                -> Maybe Color  -- ^ @Just@ a color, or @Nothing@ if parsing failed
colorFromString name = unsafePerformIO $ withCString name $ \cstr ->
                       alloca $ \colptr ->
                       if cToBool ({# call pure unsafe color_from_string #} colptr cstr)
                         then peek colptr >>= return . Just
                         else return Prelude.Nothing


-- | Returns a textual specification of color in the hexadecimal form
--   #rrggbbaa, where r, g, b and a are hex digits representing the
--   red, green, blue and alpha components respectively.
--
-- [@color@] a 'Color'
--
-- [@Returns@] a text string describing the color
--
-- * Since 0.2
--
{# fun pure unsafe color_to_string as ^ { withColor* `Color' } -> `String' peekNFreeString* #}


-- | Converts a color expressed in HLS (hue, luminance and saturation)
--   values into a 'Color'.
--
-- [@hue@] hue value, in the 0 .. 360 range
--
-- [@luminance@] luminance value, in the 0 .. 1 range
--
-- [@saturation@] saturation value, in the 0 .. 1 range
--
-- [@Returns@] a 'Color'
--
{# fun pure unsafe color_from_hls as ^
       { alloca- `Color' peek*, `Float', `Float', `Float' } -> `()' #}




-- | Converts color to the HLS format.
--
-- The hue value is in the 0 .. 360 range. The luminance and
-- saturation values are in the 0 .. 1 range.
--
-- [@color@] a 'Color'
--
-- [@Returns@] (hue, luminance, saturation)
--
{# fun pure unsafe color_to_hls as ^
       { withColor*`Color',
         alloca- `Float' peekFloatConv*,
         alloca- `Float' peekFloatConv*,
         alloca- `Float' peekFloatConv* } -> `()' #}



-- | Converts pixel from the packed representation of a four 8 bit
--   channel color to a 'Color'.
--
-- [@pixel@] a 32 bit packed integer containing a color
--
-- [@Returns@] a 'Color'
--
{# fun pure unsafe color_from_pixel as ^ { alloca- `Color' peek*, `Word32' } -> `()' #}


-- | Converts color into a packed 32 bit integer, containing all the
--   four 8 bit channels used by 'Color'.
--
-- [@color@] a 'Color'
--
-- [@Returns@] a packed color
--
{# fun pure unsafe color_to_pixel as ^ { withColor* `Color' } -> `Word32' #}

clamp low high x | x > high  = high
                 | x < low   = low
                 | otherwise = x

-- | Adds a to b and saves the resulting color inside result.
--
-- The alpha channel of result is set as as the maximum value between
-- the alpha channels of a and b.
--
-- [@a@] a 'Color'
--
-- [@b@] a 'Color'
--
-- [@Returns@] Result
--
colorAdd (Color ra ga ba aa) (Color rb gb bb ab) = let lim = clamp 0 255
                                                       r = lim (ra + rb)
                                                       g = lim (ga + gb)
                                                       b = lim (ba + bb)
                                                   in Color r g b (max aa ab)
--There's no point in marshalling all this stuff just for this
--{# fun pure unsafe color_add as ^
--       { withColor* `Color', withColor* `Color', alloca- `Color' peek* } -> `()' #}

-- | Subtracts b from a
--
-- This function assumes that the components of a are greater than the
-- components of b; the result is, otherwise, undefined.
--
-- The alpha channel of result is set as the minimum value between the
-- alpha channels of a and b.
--
-- [@a@] a 'Color'
--
-- [@b@] a 'Color'
--
-- [@Returns@] Result
--
colorSubtract (Color ra ga ba aa) (Color rb gb bb ab) = let lim = clamp 0 255
                                                            r = lim (ra - rb)
                                                            g = lim (ga - gb)
                                                            b = lim (ba - bb)
                                                        in Color r g b (min aa ab)

--{# fun pure unsafe color_subtract as ^
--       { withColor* `Color', withColor* `Color', alloca- `Color' peek* } -> `()' #}


-- | Lightens color by a fixed amount
--
-- [@color@] a 'Color'
--
-- [@Returns@] the lighter color
--
{# fun pure unsafe color_lighten as ^
       { withColor* `Color', alloca- `Color' peek* } -> `()' #}


-- | Darkens color by a fixed amount, and saves the changed color in result.
--
-- [@color@] a 'Color'
--
-- [@Returns@] the darker color.
--
{# fun pure unsafe color_darken as ^
       { withColor* `Color', alloca- `Color' peek* } -> `()' #}


-- | Shades color by factor and saves the modified color into result.
--
-- [@color@] a 'Color'
--
-- [@factor@] the shade factor to apply
--
-- [@result@] the shaded color
--
{# fun pure unsafe color_shade as ^
       { withColor* `Color', `Double', alloca- `Color' peek* } -> `()' #}

-- TODO: ClutterParamSpecColor stuff

