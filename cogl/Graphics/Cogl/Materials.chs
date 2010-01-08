-- -*-haskell-*-
--  COGL Primitives
--
--  Author : Matthew Arsenault
--
--  Created: 5 Jan 2010
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

-- | Materials — Fuctions for creating and manipulating materials
module Graphics.Cogl.Materials (
  Material,
  BlendStringError(..),
  MaterialFilter(..),
  MaterialLayerType(..),

  materialNew,
  isMaterial,   -- does it make sense to keep this?
  materialSetColor,
  materialSetColor4ub,
  materialSetColor4f,
  materialGetColor,
  materialSetAmbient,
  materialGetAmbient,
  materialSetDiffuse,
  materialGetDiffuse,
  materialSetAmbientAndDiffuse,
  materialSetEmission,
  materialGetEmission,
  materialSetSpecular,
  materialGetSpecular,
  materialSetShininess,
  materialGetShininess,
  materialSetAlphaTestFunction,

  materialSetBlend,
  materialSetBlendConstant,
  materialSetLayer,
  materialRemoveLayer,
  materialSetLayerCombine,
  materialSetLayerCombineConstant,
  materialSetLayerMatrix,
  materialGetLayers,
  materialGetNLayers,

  materialSetLayerFilters,
  materialLayerGetType,
  materialLayerGetTexture,
  materialLayerGetMinFilter,
  materialLayerGetMagFilter
) where

import C2HS
import Control.Monad (liftM)
import System.Glib.GError
import System.Glib.GList

{# import Graphics.Cogl.Types #}
{# import Graphics.Cogl.Enums #}

{# fun unsafe material_new as ^ { } -> `Material' newMaterial* #}

{# fun unsafe is_material as ^ { withMaterial* `Material' } -> `Bool' #}

{# fun unsafe material_set_color as ^ { withMaterial* `Material', withColor* `Color' } -> `()' #}

{# fun unsafe material_set_color4ub as ^
  { withMaterial* `Material', `Word8', `Word8', `Word8', `Word8' } -> `()' #}

{# fun unsafe material_set_color4f as ^
  { withMaterial* `Material', `Float', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe material_get_color as ^
  { withMaterial* `Material', allocColor- `Color' newColor* } -> `()' #}


{# fun unsafe material_set_ambient as ^ { withMaterial* `Material', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_ambient as ^
  { withMaterial* `Material', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_diffuse as ^ { withMaterial* `Material', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_diffuse as ^
  { withMaterial* `Material', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_ambient_and_diffuse as ^
  { withMaterial* `Material', withColor* `Color' } -> `()' #}

{# fun unsafe material_set_emission as ^
  { withMaterial* `Material', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_emission as ^
  { withMaterial* `Material', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_specular as ^
  { withMaterial* `Material', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_specular as ^
  { withMaterial* `Material', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_shininess as ^
  { withMaterial* `Material', `Float' } -> `()' #}

{# fun unsafe material_get_shininess as ^ { withMaterial* `Material' } -> `Float' #}

{# fun unsafe material_set_alpha_test_function as ^
  { withMaterial* `Material', cFromEnum `MaterialAlphaFunc', `Float' } -> `()' #}


materialSetBlend :: Material -> String -> IO Bool
materialSetBlend h str = let func = {# call unsafe material_set_blend #}
                         in liftM cToBool $ propagateGError $ \gerrorPtr ->
                              withMaterial h $ \hPtr ->
                                withCString str $ \strPtr ->
                                  func hPtr strPtr (castPtr gerrorPtr)



{# fun unsafe material_set_blend_constant as ^
  { withMaterial* `Material', withColor* `Color' } -> `()' #}


{# fun unsafe material_set_layer as ^
  { withMaterial* `Material', `Int', withMaterial* `Material' } -> `()' #}


{# fun unsafe material_remove_layer as ^ { withMaterial* `Material', `Int' } -> `()' #}





materialSetLayerCombine :: Material -> Int -> String -> IO Bool
materialSetLayerCombine h i str = let func = {# call unsafe material_set_layer_combine #}
                                  in liftM cToBool $ propagateGError $ \gerrorPtr ->
                                       withMaterial h $ \hPtr ->
                                         withCString str $ \strPtr ->
                                           func hPtr (cIntConv i) strPtr (castPtr gerrorPtr)

{# fun unsafe material_set_layer_combine_constant as ^
  { withMaterial* `Material', `Int', withColor* `Color' } -> `()' #}

{# fun unsafe material_set_layer_matrix as ^
  { withMaterial* `Material', `Int', withMatrix* `Matrix' } -> `()' #}


{# fun unsafe material_get_layers as ^ { withMaterial* `Material' } -> `[Material]' readMaterialList* #}

readMaterialList :: GList -> IO [Material]
readMaterialList gsl = (readGList gsl :: IO [Ptr Material]) >>= mapM (newMaterial  . castPtr)


{# fun unsafe material_get_n_layers as ^ { withMaterial* `Material' } -> `Int' #}



{# fun unsafe material_set_layer_filters as ^
  { withMaterial* `Material', `Int', cFromEnum `MaterialFilter', cFromEnum `MaterialFilter' } -> `()' #}


{# fun unsafe material_layer_get_type as ^
  { withMaterial* `Material' } -> `MaterialLayerType' cToEnum #}

{# fun unsafe material_layer_get_texture as ^ { withMaterial* `Material' } -> `Material' newMaterial* #}

{# fun unsafe material_layer_get_min_filter as ^
  { withMaterial* `Material' } -> `MaterialFilter' cToEnum #}

{# fun unsafe material_layer_get_mag_filter as ^
  { withMaterial* `Material' } -> `MaterialFilter' cToEnum #}

