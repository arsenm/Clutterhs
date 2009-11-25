-- -*-haskell-*-
--  Clutter Cairo Texture
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- | CairoTexture — Texture with Cairo integration
module Graphics.UI.Clutter.CairoTexture (
-- * Description
-- | 'CairoTexture' is a 'Texture' that displays the contents of a
-- Cairo context. The 'CairoTexture' actor will create a Cairo image
-- surface which will then be uploaded to a GL texture when needed.
--
-- 'CairoTexture' will provide a 'Cairo' context by using the
-- 'cairoTextureCreate' and 'cairoTextureCreateRegion' functions; you
-- can use the Cairo API to draw on the context.
--
-- As soon as the context is destroyed, the contents of the surface
-- will be uploaded into the 'CairoTexture' actor:
--
-- Although a new 'Cairo' is created each time you call
-- 'cairoTextureCreate' or 'cairoTextureCreateRegion', it uses the
-- same 'Surface' each time. You can call 'cairoTextureClear' to erase
-- the contents between calls.
--
-- * Warning
--
--  Note that you should never use the code above inside the "paint"
--  or "pick" virtual functions or signal handlers because it will
--  lead to performance degradation.
--
-- * Note
--
-- Since 'CairoTexture' uses a Cairo image surface internally all
-- the drawing operations will be performed in software and not using
-- hardware acceleration. This can lead to performance degradation if
-- the contents of the texture change frequently.
--
-- 'CairoTexture' is available since Clutter 1.0.
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Actor'
-- |         +----'Texture'
-- |               +----'CairoTexture'
-- @

-- * Types
  CairoTexture,
  CairoTextureClass,

-- * Constructors
  cairoTextureNew,

-- * Methods
  cairoTextureSetSurfaceSize,
  cairoTextureGetSurfaceSize,

  cairoTextureCreate,

  cairoTextureCreateRegion,
  cairoTextureClear,

  cairoSetSourceColor,

-- * Attributes
  cairoTextureSurfaceHeight,
  cairoTextureSurfaceWidth
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.Rendering.Cairo.Types (Cairo)

--TODO: CairoTextureClass

-- stop c2hs complaining about Ptr () not being Ptr Cairo
{# pointer *cairo_t as CairoPtr foreign -> Cairo nocode #}

-- | Creates a new 'CairoTexture' actor, with a surface of width by
--   height pixels.
--
-- [@width@] the width of the surface
--
-- [@height@] the height of the surface
--
-- [@Returns@] the newly created 'CairoTexture' actor
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_new as ^ { `Int', `Int' } -> `CairoTexture' newCairoTexture* #}


-- | Resizes the Cairo surface used by self to width and height.
--
-- [@self@] a 'CairoTexture'
--
-- [@width@] the new width of the surface
--
-- [@height@] the new height of the surface
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_set_surface_size as ^
       { withCairoTexture* `CairoTexture', `Int', `Int' } -> `()' #}

-- | Retrieves the surface width and height for self.
--
-- [@self@] a 'CairoTexture'
--
-- [@Returns@] (Surface width, Surface height)
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_get_surface_size as ^
       { withCairoTexture* `CairoTexture', alloca- `Int' peekIntConv*, alloca- `Int' peekIntConv* } -> `()' #}



--CHECKME: the cairo_destroy to upload the contents?
-- | Creates a new Cairo context for the cairo texture. It is similar
--   to using 'cairoTextureCreateRegion' with x_offset and y_offset of
--   0, width equal to the cairo texture surface width and height
--   equal to the cairo texture surface height.
--
-- * Warning
--
-- Do not call this function within the paint virtual function or from
-- a callback to the "paint" signal.
--
-- [@self@] a 'CairoTexture'
--
-- [@Returns@] a newly created Cairo context. Use cairo_destroy() to
-- upload the contents of the context when done drawing
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_create as ^
       { withCairoTexture* `CairoTexture' } -> `Cairo' newCairo #}


-- | Creates a new Cairo context that will updat the region defined by
--  x_offset, y_offset, width and height.
--
-- * Warning
--
-- Do not call this function within the paint virtual function or from
-- a callback to the "paint" signal.
--
-- [@self@] a 'CairoTexture'
--
-- [@x_offset@] offset of the region on the X axis
--
-- [@y_offset@] offset of the region on the Y axis
--
-- [@width@] width of the region, or -1 for the full surface width
--
-- [@height@] height of the region, or -1 for the full surface height
--
-- [@Returns@] a newly created Cairo context. Use cairo_destroy() to
-- upload the contents of the context when done drawing
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_create_region as ^
       { withCairoTexture* `CairoTexture', `Int', `Int', `Int', `Int' } -> `Cairo' newCairo #}

-- | Clears self's internal drawing surface, so that the next upload
--   will replace the previous contents of the 'CairoTexture' rather
--   than adding to it.
--
-- [@self@] a 'CairoTexture'
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_clear as ^ { withCairoTexture* `CairoTexture' } -> `()' #}

-- | Utility function for setting the source color of cr using a
--  'Color'.
--
-- [@cr@] a Cairo context
--
-- [@color@] a 'Color'
--
-- * Since 1.0
--
{# fun unsafe cairo_set_source_color as ^
       { withCairo `Cairo', withColor* `Color' } -> `()' #}


cairoTextureSurfaceHeight :: (CairoTextureClass self) => Attr self Word
cairoTextureSurfaceHeight = clutterNewAttrFromUIntProperty "surface-height"

cairoTextureSurfaceWidth :: (CairoTextureClass self) => Attr self Word
cairoTextureSurfaceWidth = clutterNewAttrFromUIntProperty "surface-width"

