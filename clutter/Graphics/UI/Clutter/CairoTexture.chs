-- -*-haskell-*-
--  Clutter Cairo Texture
--
--  Author : Matthew Arsenault
--
--  Created: 2 Oct 2009
--
--  Copyright (C) 2009-2010 Matthew Arsenault
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
-- 'renderWithCairoTexture' and 'renderWithCairoTextureRegion'
-- functions; you can use the Cairo API to draw on the context.
--
-- As soon as the context is destroyed, the contents of the surface
-- will be uploaded into the 'CairoTexture' actor:
--
-- Although a new 'Cairo' is created each time you call
-- 'renderWithCairoTexture' or 'renderWithCairoTextureRegion', it uses
-- the same 'Surface' each time. You can call 'cairoTextureClear' to
-- erase the contents between calls.
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
-- |    +----'Actor'
-- |           +----'Texture'
-- |                  +----'CairoTexture'
-- @

-- * Types
  CairoTexture,
  CairoTextureClass,

-- * Constructors
  cairoTextureNew,

-- * Methods
  cairoTextureSetSurfaceSize,
  cairoTextureGetSurfaceSize,
  cairoTextureClear,

-- * Functions for the 'Render' monad.
  renderWithCairoTexture,
  renderWithCairoTextureRegion,
  setSourceColor,

-- * Attributes
  cairoTextureSurfaceHeight,
  cairoTextureSurfaceWidth
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes

-- Cairo stuff
import Control.Exception (bracket)
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render)
import Control.Monad.Reader


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


-- | Clears self's internal drawing surface, so that the next upload
--   will replace the previous contents of the 'CairoTexture' rather
--   than adding to it.
--
-- [@self@] a 'CairoTexture'
--
-- * Since 1.0
--
{# fun unsafe cairo_texture_clear as ^ { withCairoTexture* `CairoTexture' } -> `()' #}


cairoTextureSurfaceHeight :: (CairoTextureClass self) => Attr self Word
cairoTextureSurfaceHeight = clutterNewAttrFromUIntProperty "surface-height"


cairoTextureSurfaceWidth :: (CairoTextureClass self) => Attr self Word
cairoTextureSurfaceWidth = clutterNewAttrFromUIntProperty "surface-width"



-- | Creates a new Cairo context for drawing to a 'CairoTexture'. It
-- is similar to using 'renderWithCairoTextureRegion' with x_offset and
-- y_offset of 0, width equal to the cairo texture surface width and
-- height equal to the cairo texture surface height.
--
-- * Warning
--
-- Do not call this function within the paint virtual function or from
-- a callback to the "paint" signal.
--
-- [@self@] a 'CairoTexture'
--
-- [@Context@] A new Cairo context.
--
-- * Since 1.0
--
renderWithCairoTexture :: CairoTexture -> Render a -> IO a
renderWithCairoTexture ct m =
  bracket (withCairoTexture ct $ \ctPtr ->
            liftM Cairo $ {# call unsafe cairo_texture_create #} ctPtr)
          (\context -> do status <- Cairo.Internal.status context
                          Cairo.Internal.destroy context
                          unless (status == Cairo.StatusSuccess) $
                            fail =<< Cairo.Internal.statusToString status)
          (\context -> runReaderT (Cairo.Internal.runRender m) context)


-- | Creates a new Cairo context that will update the region defined by
-- x_offset, y_offset, width and height.
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
-- * Since 1.0
--
renderWithCairoTextureRegion :: CairoTexture -> Int -> Int -> Int -> Int -> Render a -> IO a
renderWithCairoTextureRegion ct xOff yOff w h m =
  let cx = cIntConv xOff
      cy = cIntConv yOff
      cw = cIntConv w
      ch = cIntConv h
  in bracket (withCairoTexture ct $ \ctPtr ->
               liftM Cairo $ {# call unsafe cairo_texture_create_region #} ctPtr cx cy cw ch)
             (\context -> do status <- Cairo.Internal.status context
                             Cairo.Internal.destroy context
                             unless (status == Cairo.StatusSuccess) $
                              fail =<< Cairo.Internal.statusToString status)
             (\context -> runReaderT (Cairo.Internal.runRender m) context)





-- | Utility function for setting the source color of cr using a
--  'Graphics.UI.Clutter.Color'.
--
-- [@cr@] a Cairo context
--
-- [@color@] a 'Color'
--
-- * Since 1.0
--
setSourceColor :: Color -> Render ()
setSourceColor (Color r g b a) =
  if a == 0xff
    then Cairo.setSourceRGB (realToFrac r / 255.0)
                            (realToFrac g / 255.0)
                            (realToFrac b / 255.0)
    else Cairo.setSourceRGBA (realToFrac r / 255.0)
                             (realToFrac g / 255.0)
                             (realToFrac b / 255.0)
                             (realToFrac a / 255.0)

