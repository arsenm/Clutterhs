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

module Graphics.Cogl.Enums (
  FogMode(..),
  PixelFormat(..),
  BufferBit(..),
  AttributeType(..),
  FeatureFlags(..),

  TextureFlags(..),
  BlendStringError(..),
  MaterialAlphaFunc(..),
  MaterialLayerType(..),
  MaterialFilter(..),
  ShaderType(..),
) where

import C2HS
import Control.Monad (liftM)
import System.Glib.Flags


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




-- | Flags for the supported features.
--
-- [@FeatureTextureRectangle@] ARB_texture_rectangle support
--
-- [@FeatureTextureNpot@] ARB_texture_non_power_of_two support
--
-- [@FeatureTextureYuv@] ycbcr conversion support
--
-- [@FeatureTextureReadPixels@] glReadPixels() support
--
-- [@FeatureShadersGlsl@] GLSL support
--
-- [@FeatureOffscreen@] FBO support
--
-- [@FeatureOffscreenMultisample@] Multisample support on FBOs
--
-- [@FeatureOffscreenBlit@] Blit support on FBOs
--
-- [@FeatureFourClipPlanes@] At least 4 clip planes available
--
-- [@FeatureStencilBuffer@] Stencil buffer support
--
-- [@FeatureVbos@] VBO support
--
-- * Since 0.8
--
{# enum CoglFeatureFlags as FeatureFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags FeatureFlags


-- | Flags for 'readPixels'
--
-- [@ReadPixelsColorBuffer@] Read from the color buffer
--
-- * Since 1.0
--
{# enum ReadPixelsFlags as ReadPixelsFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags ReadPixelsFlags





-- | Pixel formats used by COGL.
--
-- [@PixelFormatAny@] Any format
--
-- [@PixelFormatA8@] 8 bits alpha mask
--
-- [@PixelFormatRgb565@] RGB, 16 bits
--
-- [@PixelFormatRgba4444@] RGBA, 16 bits
--
-- [@PixelFormatRgba5551@] RGBA, 16 bits
--
-- [@PixelFormatYuv@] FIXME
--
-- [@PixelFormatG8@] FIXME
--
-- [@PixelFormatRgb888@] RGB, 24 bits
--
-- [@PixelFormatBgr888@] BGR, 24 bits
--
-- [@PixelFormatRgba8888@] RGBA, 32 bits
--
-- [@PixelFormatBgra8888@] BGRA, 32 bits
--
-- [@PixelFormatArgb8888@] ARGB, 32 bits
--
-- [@PixelFormatAbgr8888@] ABGR, 32 bits
--
-- [@PixelFormatRgba8888Pre@] Premultiplied RGBA, 32 bits
--
-- [@PixelFormatBgra8888Pre@] Premultiplied BGRA, 32 bits
--
-- [@PixelFormatArgb8888Pre@] Premultiplied ARGB, 32 bits
--
-- [@PixelFormatAbgr8888Pre@] Premultiplied ABGR, 32 bits
--
-- [@PixelFormatRgba4444Pre@] Premultiplied RGBA, 16 bits
--
-- [@PixelFormatRgba5551Pre@] Premultiplied RGBA, 16 bits
--
-- * Since 0.8
--
{# enum PixelFormat as PixelFormat {underscoreToCase} deriving (Show, Eq) #}




-- | Target flags for FBOs.
--
-- [@WindowBuffer@] FIXME
--
-- [@OffscreenBuffer@] FIXME
--
-- * Since 0.8
--
{# enum CoglBufferTarget as BufferTarget {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags BufferTarget -- CHECKME:



-- | Types of auxiliary buffers
--
-- [@BufferBitColor@] Selects the primary color buffer
--
-- [@BufferBitDepth@] Selects the depth buffer
--
-- [@BufferBitStencil@] Selects the stencil buffer
--
-- * Since 1.0
--
{# enum CoglBufferBit as BufferBit {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags BufferBit


-- | Data types for the components of 'vertexBufferAdd'
--
-- [@AttributeTypeByte@] Data is the same size of a byte
--
-- [@AttributeTypeUnsignedByte@] Data is the same size of an unsigned byte
--
-- [@AttributeTypeShort@] Data is the same size of a short integer
--
-- [@AttributeTypeUnsignedShort@] Data is the same size of an unsigned short integer
--
-- [@AttributeTypeFloat@] Data is the same size of a float
--
-- * Since 1.0
--
{# enum CoglAttributeType as AttributeType {underscoreToCase} deriving (Show, Eq, Bounded) #}




-- | Flags to pass to the textureNew* family of functions.
--
-- [@TextureNone@] No flags specified
--
-- [@TextureNoAutoMipmap@] Disables the automatic generation of the
-- mipmap pyramid from the base level image whenever it is
-- updated. The mipmaps are only generated when the texture is
-- rendered with a mipmap filter so it should be free to leave out
-- this flag when using other filtering modes.
--
-- [@TextureNoSlicing@] Disables the slicing of the texture
--
-- * Since 1.0
--
{# enum CoglTextureFlags as TextureFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags TextureFlags



{# enum CoglBlendStringError as BlendStringError {underscoreToCase} deriving (Show, Eq, Bounded) #}






-- | Texture filtering is used whenever the current pixel maps either
-- to more than one texture element (texel) or less than one. These
-- filter enums correspond to different strategies used to come up
-- with a pixel color, by possibly referring to multiple neighbouring
-- texels and taking a weighted average or simply using the nearest
-- texel.
--
-- [@MaterialFilterNearest@] Measuring in manhatten distance from the,
-- current pixel center, use the nearest texture texel.
--
-- [@MaterialFilterLinear@] Use the weighted average of the 4 texels
-- nearest the current pixel center.
--
-- [@MaterialFilterNearestMipmapNearest@] Select the mimap level whose
-- texel size most closely matches the current pixel, and use the
-- 'MaterialFilterNearest' criterion.
--
-- [@MaterialFilterLinearMipmapNearest@] Select the mimap level whose
-- texel size most closely matches the current pixel, and use the
-- 'MaterialFilterLinear' criterion.
--
-- [@MaterialFilterNearestMipmapLinear@] Select the two mimap levels
-- whose texel size most closely matches the current pixel, use the
-- 'MaterialFilterNearest' criterion on each one and take their
-- weighted average.
--
-- [@MaterialFilterLinearMipmapLinear@] Select the two mimap levels
-- whose texel size most closely matches the current pixel, use the
-- 'MaterialFilterLinear' criterion on each one and take their
-- weighted average.
--
{# enum CoglMaterialFilter as MaterialFilter {underscoreToCase} deriving (Show, Eq, Bounded) #}




-- | Types of shaders
--
-- [@ShaderTypeVertex@] A program for proccessing vertices
--
-- [@ShaderTypeFragment@] A program for processing fragments
--
-- * Since 1.0
--
{# enum CoglShaderType as ShaderType {underscoreToCase} deriving (Show, Eq, Bounded) #}




-- | Alpha testing happens before blending primitives with the
-- framebuffer and gives an opportunity to discard fragments based on
-- a comparison with the incoming alpha value and a reference alpha
-- value. The 'MaterialAlphaFunc' determines how the comparison is
-- done.
--
-- [@MaterialAlphaFuncNever@] Never let the fragment through.
--
-- [@MaterialAlphaFuncLess@] Let the fragment through if the incoming
-- alpha value is less than the reference alpha value.
--
-- [@MaterialAlphaFuncEqual@] Let the fragment through if the incoming
-- alpha value equals the reference alpha value.
--
-- [@MaterialAlphaFuncLequal@] Let the fragment through if the
-- incoming alpha value is less than or equal to the reference alpha
-- value.
--
-- [@MaterialAlphaFuncGreater@] Let the fragment through if the
-- incoming alpha value is greater than the reference alpha value.
--
-- [@MaterialAlphaFuncNotequal@] Let the fragment through if the
-- incoming alpha value does not equal the reference alpha value.
--
-- [@MaterialAlphaFuncGequal@] Let the fragment through if the
-- incoming alpha value is greater than or equal to the reference
-- alpha value.
--
-- [@MaterialAlphaFuncAlways@] Always let the fragment through.
--
{# enum CoglMaterialAlphaFunc as MaterialAlphaFunc {underscoreToCase} deriving (Show, Eq, Bounded) #}


-- | Available types of layers for a 'Material'. This enumeration
-- might be expanded in later versions.
--
-- * Since 1.0
--
{# enum CoglMaterialLayerType as MaterialLayerType {underscoreToCase} deriving (Show, Eq, Bounded) #}

