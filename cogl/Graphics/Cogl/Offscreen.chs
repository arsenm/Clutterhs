-- -*-haskell-*-
--  COGL Offscreen Buffers
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

-- | Offscreen Buffers — Fuctions for creating and manipulating
-- offscreen frame buffer objects
module Graphics.Cogl.Offscreen (
  Offscreen,

  offscreenNewToTexture,

  setDrawBuffer,
  popDrawBuffer,
  pushDrawBuffer
) where

import C2HS


{# import Graphics.Cogl.Types #}
{# import Graphics.Cogl.Enums #}

--{# fun unsafe is_offscreen

{# fun unsafe offscreen_new_to_texture as ^
  { withOffscreen* `Offscreen' } -> `Texture' newTexture* #}

-- If handle is nothing, window buffer
setDrawBuffer :: Maybe Offscreen -> IO ()
setDrawBuffer Nothing = {# call unsafe set_draw_buffer #} (cFromEnum WindowBuffer) nullPtr
setDrawBuffer (Just h) = withOffscreen h $ \hPtr ->
  {# call unsafe set_draw_buffer #} (cFromEnum OffscreenBuffer) hPtr


{# fun unsafe pop_draw_buffer as ^ { } -> `()' #}

{# fun unsafe push_draw_buffer as ^ { } -> `()' #}




