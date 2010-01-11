-- -*-haskell-*-
--  COGL General
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

-- | General API — General purpose API
module Graphics.Cogl.General (
  getFeatures,
  featuresAvailable,
  checkExtension,

  pushMatrix,
  popMatrix,
  scale,
  translate,
  rotate,
  frustum,
  perspective,
  ortho,

  getModelviewMatrix,
  setModelviewMatrix,
  getProjectionMatrix,
  setProjectionMatrix,

  viewport,
  getViewport,
  clear,
  getBitmasks,
  setDepthTestEnabled,
  getDepthTestEnabled,
  setBackfaceCullingEnabled,
  getBackfaceCullingEnabled,

  setFog,
  disableFog,
  setSource,
  setSourceColor,
  setSourceColor4ub,
  setSourceColor4f,
  setSourceTexture,
--readPixels,

  flush,
  beginGl,
  endGl
) where

import C2HS hiding  (rotate)
import Control.Monad (liftM)

import System.Glib.Flags

{# import Graphics.Cogl.Types #}
{# import Graphics.Cogl.Enums #}


cFromFlags :: (Flags a) => [a] -> CInt
cFromFlags = cIntConv . fromFlags

cToFlags :: (Flags a) => CInt ->  [a]
cToFlags = toFlags . cIntConv



{# fun unsafe get_features as ^ { } -> `[FeatureFlags]' cToFlags #}

{# fun unsafe features_available as ^ { cFromFlags `[FeatureFlags]' } -> `Bool' #}


{# fun unsafe check_extension as ^ { `String', `String' } -> `Bool' #}


-- get_proc_address

-- get_option_group

{# fun unsafe push_matrix as ^ { } -> `()' #}
{# fun unsafe pop_matrix as ^ { } -> `()' #}

{# fun unsafe scale as ^ { `Float', `Float', `Float' } -> `()' #}
{# fun unsafe translate as ^ { `Float', `Float', `Float' } -> `()' #}

{# fun unsafe rotate as ^ { `Float', `Float', `Float', `Float' } -> `()' #}


{# fun unsafe frustum as ^ { `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}


{# fun unsafe perspective as ^ { `Float', `Float', `Float', `Float' } -> `()' #}


{# fun unsafe ortho as ^ { `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe get_modelview_matrix as ^ { withMatrix* `Matrix' } -> `()' #}
{# fun unsafe set_modelview_matrix as ^ { withMatrix* `Matrix' } -> `()' #}

{# fun unsafe get_projection_matrix as ^ { withMatrix* `Matrix' } -> `()' #}
{# fun unsafe set_projection_matrix as ^ { withMatrix* `Matrix' } -> `()' #}

{# fun unsafe viewport as ^ { cIntConv `Word', cIntConv `Word' } -> `()' #}


--CHECKME: Order of stuff returned
getViewport :: IO (Float, Float, Float, Float)
getViewport = allocaArray 4 $ \arrPtr -> do
  {# call unsafe get_viewport #} arrPtr
  [a,b,c,d] <- liftM (map cFloatConv) (peekArray 4 arrPtr)
  return (a,b,c,d)

longFromFlags :: (Flags a) => [a] -> CULong
longFromFlags = cIntConv . fromFlags

{# fun unsafe clear as ^ { withColor* `Color', longFromFlags `[BufferBit]' } -> `()' #}

{# fun unsafe get_bitmasks as ^
   { alloca- `Int' peekIntConv*,
     alloca- `Int' peekIntConv*,
     alloca- `Int' peekIntConv*,
     alloca- `Int' peekIntConv* } -> `()' #}

{# fun unsafe set_depth_test_enabled as ^ { `Bool' } -> `()' #}

{# fun unsafe get_depth_test_enabled as ^ { } -> `Bool' #}


{# fun unsafe set_backface_culling_enabled as ^ { `Bool' } -> `()' #}

{# fun unsafe get_backface_culling_enabled as ^ { } -> `Bool' #}


{# fun unsafe set_fog as ^
   { withColor* `Color', cFromEnum `FogMode', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe disable_fog as ^ { } -> `()' #}

-- wtf cast.
withHandleCast :: Handle -> (Ptr () -> IO a) -> IO a

withHandleCast h f = withHandle h (f . castPtr)

{# fun unsafe set_source as ^ { withHandleCast* `Handle' } -> `()' #}

{# fun unsafe set_source_color as ^ { withColor* `Color' } -> `()' #}


{# fun unsafe set_source_color4ub as ^ { `Word8', `Word8', `Word8', `Word8' } -> `()' #}


{# fun unsafe set_source_color4f as ^ { `Float', `Float', `Float', `Float' } -> `()' #}


{# fun unsafe set_source_texture as ^ { withHandleCast* `Handle' } -> `()' #}


-- read_pixels


{# fun unsafe flush as ^ { } -> `()' #}

{# fun unsafe begin_gl as ^ { } -> `()' #}

{# fun unsafe end_gl as ^ { } -> `()' #}



