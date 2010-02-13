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


-- | Creates identity matrix:
--
-- .xx=1; .xy=0; .xz=0; .xw=0;
-- .yx=0; .yy=1; .yz=0; .yw=0;
-- .zx=0; .zy=0; .zz=1; .zw=0;
-- .wx=0; .wy=0; .wz=0; .ww=1;
--
matrixNewInitIdentity :: IO Matrix
matrixNewInitIdentity = do allocMatrix $ \m -> do
                             {# call unsafe matrix_init_identity #} m
                             newMatrix m


-- | Resets matrix to the identity matrix:
--
-- .xx=1; .xy=0; .xz=0; .xw=0;
-- .yx=0; .yy=1; .yz=0; .yw=0;
-- .zx=0; .zy=0; .zz=1; .zw=0;
-- .wx=0; .wy=0; .wz=0; .ww=1;
--
-- [@matrix@] A 4x4 transformation matrix
--
{# fun unsafe matrix_init_identity as ^ { withMatrix* `Matrix' } -> `()' #}

-- | Multiplies the matrix by the given frustum perspective matrix.
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@left@] coord of left vertical clipping plane
--
-- [@right@] coord of right vertical clipping plane
--
-- [@bottom@] coord of bottom horizontal clipping plane
--
-- [@top@] coord of top horizontal clipping plane
--
-- [@z_near@] positive distance to near depth clipping plane
--
-- [@z_far@] positive distance to far depth clipping plane
--
{# fun unsafe matrix_frustum as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}



-- | Multiples the matrix by a parallel projection matrix.
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@left@] The coordinate for the left clipping plane
--
-- [@right@] The coordinate for the right clipping plane
--
-- [@bottom@] The coordinate for the bottom clipping plane
--
-- [@top@] The coordinate for the top clipping plane
--
-- [@z_near@] The coordinate for the near clipping plane (may be
-- negative if the plane is behind the viewer)
--
-- [@z_far@] The coordinate for the far clipping plane (may be
-- negative if the plane is behind the viewer)
--
{# fun unsafe matrix_ortho as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}



-- | Multiplies the matrix by the described perspective matrix
--
-- Note: you should be careful not to have to great a z_far / z_near
-- ratio since that will reduce the effectiveness of depth testing
-- since there wont be enough precision to identify the depth of
-- objects near to each other.
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@fov_y@] A field of view angle for the Y axis
--
-- [@aspect@] The ratio of width to height determining the field of
-- view angle for the x axis.
--
-- [@z_near@] The distance to the near clip plane. Never pass 0 and
-- always pass a positive number.
--
-- [@z_far@] The distance to the far clip plane. (Should always be
-- positive)
--
{# fun unsafe matrix_perspective as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float' } -> `()' #}

--TODO: Don't document this
allocaInit :: Storable a => a -> (Ptr a -> IO b) -> IO b
allocaInit val f = alloca $ \ptr -> poke ptr val >> f ptr



-- | This transforms a point whos position is given and returned as
-- four float components.
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@x@] The X component of your points position [in:out]
--
-- [@y@] The Y component of your points position [in:out]
--
-- [@z@] The Z component of your points position [in:out]
--
-- [@w@] The W component of your points position [in:out]
--
matrixTransformPoint :: Matrix -> (Float, Float, Float, Float) -> IO (Float, Float, Float, Float)
matrixTransformPoint mat (x,y,z,w) =
  withMatrix mat $ \matPtr ->
    allocaInit (cFloatConv x) $ \xPtr ->
      allocaInit (cFloatConv y) $ \yPtr ->
        allocaInit (cFloatConv z) $ \zPtr ->
          allocaInit (cFloatConv w) $ \wPtr -> do
            {# call unsafe matrix_transform_point #} matPtr xPtr yPtr zPtr wPtr
            liftM4 (,,,) (peekFloatConv xPtr) (peekFloatConv yPtr) (peekFloatConv zPtr) (peekFloatConv wPtr)


-- | This function multiples the two supplied matricies together and
-- returns the result
--
-- [@a@] A 4x4 transformation matrix
--
-- [@b@] A 4x4 transformation matrix
--
-- [@Returns@] a 4x4 matrix to store the result in
--
{# fun unsafe matrix_multiply as ^
  { allocMatrix- `Matrix', withMatrix* `Matrix', withMatrix* `Matrix' } -> `()' #}


-- | This function multiples your matrix with a rotation matrix that
-- applies a rotation of angle degrees around the specified 3D vector.
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@angle@] The angle you want to rotate in degrees
--
-- [@x@] X component of your rotation vector
--
-- [@y@] Y component of your rotation vector
--
-- [@z@] Z component of your rotation vectro
--
{# fun unsafe matrix_rotate as ^
  { withMatrix* `Matrix', `Float', `Float', `Float', `Float' } -> `()' #}



{# fun unsafe matrix_translate as ^
  { withMatrix* `Matrix', `Float', `Float', `Float' } -> `()' #}


-- | This function multiples your matrix with a transform matrix that
-- scales along the X, Y and Z axis.
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@sx@] The X scale factor
--
-- [@sy@] The Y scale factor
--
-- [@sz@] The Z scale factor
--
{# fun unsafe matrix_scale as ^
  { withMatrix* `Matrix', `Float', `Float', `Float' } -> `()' #}


withArrayCast xs f = withArray xs (f . castPtr)



-- | This initialises matrix with the contents of array
--
-- [@matrix@] A 4x4 transformation matrix
--
-- [@array@] A linear array of 16 floats (column-major order)
--
{# fun unsafe matrix_init_from_array as matrixInitFromList
  { withMatrix* `Matrix', withArrayCast* `[Float]' } -> `()' #}

-- get_array


