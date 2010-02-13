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


-- | This creates an offscreen buffer object using the given texture
-- as the primary color buffer. It doesn't just initialize the
-- contents of the offscreen buffer with the texture; they are tightly
-- bound so that drawing to the offscreen buffer effectivly updates
-- the contents of the given texture. You don't need to destroy the
-- offscreen buffer before you can use the texture again.
--
-- Note: This does not work with sliced Cogl textures.
--
-- [@handle@] A CoglHandle for a Cogl texture
--
-- [@Returns@] @Just@ a CoglHandle for the new offscreen buffer or
-- @Nothing@ if it wasn't possible to create the buffer.
--
{# fun unsafe offscreen_new_to_texture as ^
  { withOffscreen* `Offscreen' } -> `Maybe Texture' maybeNewTexture* #}


-- | This redirects all subsequent drawing to the specified draw
-- buffer. This can either be an offscreen buffer created with
-- 'offscreenNewToTexture' or you can revert to your original on
-- screen window buffer.
--
-- [@offscreen@] If you are setting a draw buffer of type
-- COGL_OFFSCREEN_BUFFER then this is @Just@ a CoglHandle for the offscreen
-- buffer. @Nothing@ for a window buffer.
--
setDrawBuffer :: Maybe Offscreen -> IO ()
setDrawBuffer Nothing = {# call unsafe set_draw_buffer #} (cFromEnum WindowBuffer) nullPtr
setDrawBuffer (Just h) = withOffscreen h $ \hPtr ->
  {# call unsafe set_draw_buffer #} (cFromEnum OffscreenBuffer) hPtr


-- | Restore 'setDrawBuffer' state.
{# fun unsafe pop_draw_buffer as ^ { } -> `()' #}

-- | Save 'setDrawBuffer' state.
{# fun unsafe push_draw_buffer as ^ { } -> `()' #}


