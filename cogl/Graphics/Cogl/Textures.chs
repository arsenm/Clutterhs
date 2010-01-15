-- -*-haskell-*-
--  COGL Textures
--
--  Author : Matthew Arsenault
--
--  Created: 7 Jan 2010
--
--  Copyright (C) 2010 Matthew Arsenault
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


-- | Textures — Fuctions for creating and manipulating textures
module Graphics.Cogl.Textures (
  Texture,

  textureNewWithSize,
  textureNewFromFile,
--textureNewFromData,
--textureNewFromForeign,
--textureNewFromBitmap,

  textureGetWidth,
  textureGetHeight,
  textureGetFormat,
  textureGetRowstride,
  textureGetMaxWaste,
  textureIsSliced,
--textureGetGlTexture,
--textureGetData,
--textureSetRegion
) where

import C2HS
import System.Glib.Flags
import System.Glib.GError

{# import Graphics.Cogl.Types #}
{# import Graphics.Cogl.Enums #}

-- cFromFlags :: (Flags a) => [a] -> CInt
cFromFlags = cIntConv . fromFlags

{# fun unsafe texture_new_with_size as ^
  { cIntConv `Word',
    cIntConv `Word',
    cFromFlags `[TextureFlags]',
    cFromEnum `PixelFormat' } -> `Texture' newTexture* #}

textureNewFromFile :: String -> [TextureFlags] -> PixelFormat -> IO Texture
textureNewFromFile fn flags pf = let func = {# call unsafe texture_new_from_file #}
                                     cf = cFromFlags flags
                                     cpf = cFromEnum pf
                                 in withCString fn $ \strPtr ->
                                      propagateGError $ \gerrorPtr ->
                                        newTexture =<< func strPtr cf cpf gerrorPtr

-- {# texture_new_from_data
-- {# texture_new_from_foreign
-- {# texture_new_from_bitmap
-- {# is_texture

{# fun unsafe texture_get_width as ^ { withTexture* `Texture' } -> `Word' cIntConv #}

{# fun unsafe texture_get_height as ^ { withTexture* `Texture' } -> `Word' cIntConv #}

{# fun unsafe texture_get_format as ^
  { withTexture* `Texture' } -> `PixelFormat' cToEnum #}

{# fun unsafe texture_get_rowstride as ^ { withTexture* `Texture' } -> `Word' cIntConv #}

{# fun unsafe texture_get_max_waste as ^ { withTexture* `Texture' } -> `Int' #}

{# fun unsafe texture_is_sliced as ^ { withTexture* `Texture' } -> `Bool' cToBool #}



-- {# fun unsafe texture_get_gl_texture
-- texture_get_data
-- texture_set_region

