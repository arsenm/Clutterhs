-- -*-haskell-*-
--  COGL Primitives
--
--  Author : Matthew Arsenault
--
--  Created: 18 Dec 2009
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

module Graphics.Cogl.Primitives (
  rectangle,
  rectangles,
{-
  rectangleWithTextureCoords,
  rectanglesWithTextureCoords,
  rectangleWithMultitextureCoords,
  polygon,
  pathNew,
  pathMoveTo,
  pathClose,
  pathLineTo,
  pathCurveTo,
  pathArc,
  pathRelMoveTo,
  pathRelLineTo,
  pathRelCurveTo,
  pathLine,
  pathPolyline,
  pathPolygon,
  pathRectangle,
  pathRoundRectangle,
  pathEllipse,
  pathFill,
  pathFillPreserve,
  pathStroke,
  pathStrokePreserve,
  color
-}
) where

import C2HS
import Control.Monad (zipWithM_)

{# import Graphics.Cogl.Types #}


{# fun unsafe rectangle as ^
  { `Float', `Float', `Float', `Float' } -> `()' #}


--TODO: easier to make tuple storable
rectangles :: [(Float, Float, Float, Float)] -> IO ()
rectangles verts = let len = length verts
                       pokeTup ptr i (a,b,c,d) = do
                         pokeElemOff ptr i a
                         pokeElemOff ptr (i+1) b
                         pokeElemOff ptr (i+2) c
                         pokeElemOff ptr (i+3) d
                   in do
                     allocaArray (len * 4) $ \ptr ->
                       zipWithM_ (pokeTup ptr) [0,4..] verts


