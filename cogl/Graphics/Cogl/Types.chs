-- -*-haskell-*-
--  COGL Types
--
--  Author : Matthew Arsenault
--
--  Created: 13 Dec 2009
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

{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

#include <cogl/cogl.h>

{# context lib="cogl" prefix="cogl" #}

module Graphics.Cogl.Types (
  module Data.Word,
  Handle,
  withHandle,
  newHandle,

  Material,
  withMaterial,
  newMaterial,

  VertexBuffer,
  withVertexBuffer,
  newVertexBuffer,

  Offscreen,
  withOffscreen,
  newOffscreen,

  VertexIndices,
  mkVertexIndices,
  withVertexIndices,
  newVertexIndicesRaw,

  Color,
  allocColor,
  newColor,
  withColor,

  Matrix,
  withMatrix,
  newMatrix
) where

import C2HS
import Control.Monad (liftM)

import Data.Word

-- *** Handle

-- what I mean is
-- {# pointer *CoglHandle as Handle foreign newtype #}
--
-- but then everything complains of expecting Ptr () and getting Ptr
-- Handle, so cast. It might
-- it might be because that CoglHandle is just a typedef for gpointer

newtype Handle = Handle (ForeignPtr Handle)

withHandle :: Handle -> (Ptr () -> IO b) -> IO b
withHandle (Handle fptr) = withForeignPtr (castForeignPtr fptr)

newHandle :: Ptr () -> IO Handle
newHandle = liftM Handle . newForeignPtr handleUnref . castPtr

foreign import ccall unsafe "&cogl_handle_unref"
  handleUnref :: FinalizerPtr Handle

-- *** Color
{# pointer *CoglColor as Color foreign newtype #}

newColor :: Ptr Color -> IO Color
newColor = liftM Color . newForeignPtr colorFree


foreign import ccall unsafe "&cogl_color_free"
  colorFree :: FinalizerPtr Color

allocColor :: (Ptr Color -> IO a) -> IO a
allocColor act = act =<< mallocBytes {# sizeof CoglColor #}


-- *** Matrix

{# pointer *CoglMatrix as Matrix foreign newtype #}

--CHECKME: Free
newMatrix :: Ptr Matrix -> IO Matrix
newMatrix = liftM Matrix . newForeignPtr finalizerFree


-- *** Material

-- same issue as for Handle and others.

newtype Material = Material (ForeignPtr Material)

withMaterial :: Material -> (Ptr () -> IO b) -> IO b
withMaterial (Material fptr) = withForeignPtr (castForeignPtr fptr)

newMaterial :: Ptr () -> IO Material
newMaterial = liftM Material . newForeignPtr materialUnref . castPtr

foreign import ccall unsafe "&cogl_material_unref"
  materialUnref :: FinalizerPtr Material

-- *** VertexBuffer

-- same issue as for Handle and others.

newtype VertexBuffer = VertexBuffer (ForeignPtr VertexBuffer)

withVertexBuffer :: VertexBuffer -> (Ptr () -> IO b) -> IO b
withVertexBuffer (VertexBuffer fptr) = withForeignPtr (castForeignPtr fptr)

newVertexBuffer :: Ptr () -> IO VertexBuffer
newVertexBuffer = liftM VertexBuffer . newForeignPtr vertexBufferUnref . castPtr

foreign import ccall unsafe "&cogl_vertex_buffer_unref"
  vertexBufferUnref :: FinalizerPtr VertexBuffer

-- *** VertexIndices

data VertexIndices a = VertexIndices a VertexIndicesRaw

newtype VertexIndicesRaw = VertexIndicesRaw (ForeignPtr VertexIndicesRaw)

withVertexIndices :: VertexIndices a -> (Ptr () -> IO b) -> IO b
withVertexIndices (VertexIndices _ (VertexIndicesRaw fptr)) = withForeignPtr (castForeignPtr fptr)

newVertexIndicesRaw :: Ptr () -> IO VertexIndicesRaw
newVertexIndicesRaw = liftM VertexIndicesRaw . newForeignPtr vertexIndicesUnref . castPtr

mkVertexIndices :: a -> VertexIndicesRaw -> VertexIndices a
mkVertexIndices _ raw = VertexIndices (undefined :: a) raw

foreign import ccall unsafe "&cogl_handle_unref"
  vertexIndicesUnref :: FinalizerPtr VertexIndicesRaw



-- *** OffscreenBuffer

newtype Offscreen = Offscreen (ForeignPtr Offscreen)

withOffscreen :: Offscreen -> (Ptr () -> IO b) -> IO b
withOffscreen (Offscreen fptr) = withForeignPtr (castForeignPtr fptr)

newOffscreen :: Ptr () -> IO Offscreen
newOffscreen = liftM Offscreen . newForeignPtr vertexUnref . castPtr

foreign import ccall unsafe "&cogl_offscreen_unref"
  offscreenUnref :: FinalizerPtr Offscreen

