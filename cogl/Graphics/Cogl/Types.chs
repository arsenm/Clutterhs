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

{# pointer *CoglHandle as Handle foreign newtype #}

newHandle :: Ptr Handle -> IO Handle
newHandle = liftM Handle . newForeignPtr handleUnref

foreign import ccall unsafe "&cogl_hangle_unref"
  handleUnref :: FinalizerPtr Handle

-- *** Color
{# pointer *CoglColor as Color foreign newtype #}

newColor :: Ptr Color -> IO Color
newColor = liftM Color . newForeignPtr colorFree


foreign import ccall unsafe "&cogl_color_free"
  colorFree :: FinalizerPtr Color


-- *** Matrix

{# pointer *CoglMatrix as Matrix foreign newtype #}

--CHECKME: Free
newMatrix :: Ptr Matrix -> IO Matrix
newMatrix = liftM Matrix . newForeignPtr finalizerFree


