-- -*-haskell-*-
--  COGL Matrices
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


-- | Matrices — Fuctions for initializing and manipulating 4x4
-- matrices.
module Graphics.Cogl.Matrices (
  matrixInitIdentity,
  matrixFrustum,
  matrixOrtho,
  matrixPerspective,
  matrixTransformPoint,
  matrixMultiply,
  matrixRotate,
  matrixTranslate,
  matrixScale,
  matrixInitFromList,
--matrixGetArray
) where

import C2HS
import Control.Monad (liftM4)

{# import Graphics.Cogl.Types #}


{# fun unsafe matrix_init_identity as ^ { allocMatrix- `Matrix' } -> `()' #}

{# fun unsafe matrix_frustum as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe matrix_ortho as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe matrix_perspective as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float' } -> `()' #}

allocaInit :: Storable a => a -> (Ptr a -> IO b) -> IO b
allocaInit val f = alloca $ \ptr -> poke ptr val >> f ptr

matrixTransformPoint :: Matrix -> (Float, Float, Float, Float) -> IO (Float, Float, Float, Float)
matrixTransformPoint mat (x,y,z,w) =
  withMatrix mat $ \matPtr ->
    allocaInit (cFloatConv x) $ \xPtr ->
      allocaInit (cFloatConv y) $ \yPtr ->
        allocaInit (cFloatConv z) $ \zPtr ->
          allocaInit (cFloatConv w) $ \wPtr -> do
            {# call unsafe matrix_transform_point #} matPtr xPtr yPtr zPtr wPtr
            liftM4 (,,,) (peekFloatConv xPtr) (peekFloatConv yPtr) (peekFloatConv zPtr) (peekFloatConv wPtr)


{# fun unsafe matrix_multiply as ^
  { allocMatrix- `Matrix', withMatrix* `Matrix', withMatrix* `Matrix' } -> `()' #}


{# fun unsafe matrix_rotate as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe matrix_translate as ^
  { withMatrix* `Matrix', `Float', `Float', `Float' } -> `()' #}


{# fun unsafe matrix_scale as ^
  { withMatrix* `Matrix', `Float', `Float', `Float' } -> `()' #}


withArrayCast xs f = withArray xs (f . castPtr)

{# fun unsafe matrix_init_from_array as matrixInitFromList
  { withMatrix* `Matrix', withArrayCast* `[Float]' } -> `()' #}

-- get_array


