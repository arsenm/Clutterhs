-- -*-haskell-*-
--  Clutter Rectangle
--
--  Author : Matthew Arsenault
--
--  Created: 2 Oct 2009
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

module Graphics.UI.Clutter.CairoTexture (
                                         cairoTextureNew,
                                         cairoTextureSetSurfaceSize,
                                         cairoTextureGetSurfaceSize,
                                       --cairoTextureSurfaceSize, --TODO: Pair
                                       --cairoTextureCreate,
                                       --cairoTextureCreateRegion,
                                       --cairoTextureClear,
                                       --cairoSetSourceColor
                                       --cairoSourceColor --TODO write only attr
                                        ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe cairo_texture_new as ^ { `Int', `Int' } -> `CairoTexture' newCairoTexture* #}

{# fun unsafe cairo_texture_set_surface_size as ^
       { withCairoTexture* `CairoTexture', `Int', `Int' } -> `()' #}
{# fun unsafe cairo_texture_get_surface_size as ^
       { withCairoTexture* `CairoTexture', alloca- `Int' peekIntConv*, alloca- `Int' peekIntConv* } -> `()' #}


{-
--TODO: Lookup cairo types
{# fun unsafe cairo_texture_create as ^
       { withCairoTexture* `CairoTexture' } -> `CairoT' #}
{# fun unsafe cairo_texture_create_region as ^
       { withCairoTexture* `CairoTexture', `Int', `Int', `Int', `Int' } -> `CairoT' #}

-}

{# fun unsafe cairo_texture_clear as ^ { withCairoTexture* `CairoTexture' } -> `()' #}

{-
{# fun unsafe cairo_set_source_color as ^
       { withCairo* `CairoT', withColor* `Color' } -> `()' #}
-}

