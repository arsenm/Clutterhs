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
  Handle,
  FogMode(..),
  Color,
  newColor,
  withColor
) where

import C2HS
import Control.Monad (liftM)

-- *** Handle

{# pointer *CoglHandle as Handle foreign newtype #}

newHandle :: Ptr Handle -> IO Handle
newHandle = liftM Handle . newForeignPtr handleUnref

foreign import ccall unsafe "&cogl_hangle_unref"
  handleUnref :: FinalizerPtr Handle


-- | The fog mode determines the equation used to calculate the
-- fogging blend factor while fogging is enabled. The simplest
-- 'FogModeLinear' mode determines f as:
--
-- >  f = end - eye_distance \/ end - start
--
-- Where eye_distance is the distance of the current fragment in eye
-- coordinates from the origin.
--
-- [@FogModeLinear@] Calculates the fog blend factor as: >  f = end - eye_distance \/ end - start
--
-- [@FogModeExponential@] Calculates the fog blend factor as: >   f = e ^ -(density * eye_distance)
--
-- [@FogModeExponentialSquared@] Calculates the fog blend factor as: >  f = e ^ -(density * eye_distance)^2
--
-- * Since 1.0
--
{# enum CoglFogMode as FogMode {underscoreToCase} deriving (Show, Eq) #}


-- *** Color
{# pointer *CoglColor as Color foreign newtype #}

newColor :: Ptr Color -> IO Color
newColor = liftM Color . newForeignPtr colorFree


foreign import ccall unsafe "&cogl_color_free"
  colorFree :: FinalizerPtr Color

