-- -*-haskell-*-
--  COGL Types
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

module Graphics.Cogl.Types (
  module Data.Word,
  Handle,
  withHandle,
  newHandle,

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

