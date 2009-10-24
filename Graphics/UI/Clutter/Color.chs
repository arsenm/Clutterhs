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

module Graphics.UI.Clutter.Color (
                                  colorFromString,

                                  colorToString,
                                --colorNew,
                                --colorFree,
                                --colorCopy,

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
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import Data.Word
import System.Glib.GType (GType, typeInstanceIsA)
import System.Glib.GObject
import System.Glib.GValue (GValue(GValue))

--FIXME: Check this for ok unsafePerformIO. Ceiling cat is watching.
colorFromString::String -> Maybe Color
colorFromString name = unsafePerformIO $ withCString name $ \cstr ->
                       alloca $ \colptr ->
                       if cToBool ({# call pure unsafe color_from_string #} colptr cstr)
                         then peek colptr >>= return . Just
                         else return Prelude.Nothing

{# fun pure unsafe color_to_string as ^ { withColor* `Color' } -> `String' #}

{# fun pure unsafe color_from_hls as ^
       { alloca- `Color' peek*, `Float', `Float', `Float' } -> `()' #}

{# fun pure unsafe color_to_hls as ^
       { withColor*`Color',
         alloca- `Float' peekFloatConv*,
         alloca- `Float' peekFloatConv*,
         alloca- `Float' peekFloatConv* } -> `()' #}

{# fun pure unsafe color_from_pixel as ^ { alloca- `Color' peek*, `Word32' } -> `()' #}
{# fun pure unsafe color_to_pixel as ^ { withColor* `Color' } -> `Word32' #}

{# fun pure unsafe color_add as ^
       { withColor* `Color', withColor* `Color', alloca- `Color' peek* } -> `()' #}

{# fun pure unsafe color_subtract as ^
       { withColor* `Color', withColor* `Color', alloca- `Color' peek* } -> `()' #}

--CHECKME: Are these pure? Probably.
{# fun pure unsafe color_lighten as ^
       { withColor* `Color', alloca- `Color' peek* } -> `()' #}
{# fun pure unsafe color_darken as ^
       { withColor* `Color', alloca- `Color' peek* } -> `()' #}

{# fun pure unsafe color_shade as ^
       { withColor* `Color', `Double', alloca- `Color' peek* } -> `()' #}

--TODO: ClutterParamSpecColor stuff

