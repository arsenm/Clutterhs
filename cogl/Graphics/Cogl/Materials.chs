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
  BlendStringError(..),
  MaterialFilter(..),
  MaterialLayerType(..),

  materialNew,
  isMaterial,
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

--FIXME: I wasn't paying attention and these should be using Material,
--not Handle

{# fun unsafe material_new as ^ { } -> `Handle' newHandle* #}

{# fun unsafe is_material as ^ { withHandle* `Handle' } -> `Bool' #}

{# fun unsafe material_set_color as ^ { withHandle* `Handle', withColor* `Color' } -> `()' #}

{# fun unsafe material_set_color4ub as ^
  { withHandle* `Handle', `Word8', `Word8', `Word8', `Word8' } -> `()' #}

{# fun unsafe material_set_color4f as ^
  { withHandle* `Handle', `Float', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe material_get_color as ^
  { withHandle* `Handle', allocColor- `Color' newColor* } -> `()' #}


{# fun unsafe material_set_ambient as ^ { withHandle* `Handle', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_ambient as ^
  { withHandle* `Handle', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_diffuse as ^ { withHandle* `Handle', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_diffuse as ^
  { withHandle* `Handle', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_ambient_and_diffuse as ^
  { withHandle* `Handle', withColor* `Color' } -> `()' #}

{# fun unsafe material_set_emission as ^
  { withHandle* `Handle', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_emission as ^
  { withHandle* `Handle', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_specular as ^
  { withHandle* `Handle', withColor* `Color' } -> `()' #}

{# fun unsafe material_get_specular as ^
  { withHandle* `Handle', allocColor- `Color' newColor* } -> `()' #}

{# fun unsafe material_set_shininess as ^
  { withHandle* `Handle', `Float' } -> `()' #}

{# fun unsafe material_get_shininess as ^ { withHandle* `Handle' } -> `Float' #}

{# fun unsafe material_set_alpha_test_function as ^
  { withHandle* `Handle', cFromEnum `MaterialAlphaFunc', `Float' } -> `()' #}


materialSetBlend :: Handle -> String -> IO Bool
materialSetBlend h str = let func = {# call unsafe material_set_blend #}
                         in liftM cToBool $ propagateGError $ \gerrorPtr ->
                              withHandle h $ \hPtr ->
                                withCString str $ \strPtr ->
                                  func hPtr strPtr (castPtr gerrorPtr)



{# fun unsafe material_set_blend_constant as ^
  { withHandle* `Handle', withColor* `Color' } -> `()' #}


{# fun unsafe material_set_layer as ^
  { withHandle* `Handle', `Int', withHandle* `Handle' } -> `()' #}


{# fun unsafe material_remove_layer as ^ { withHandle* `Handle', `Int' } -> `()' #}





materialSetLayerCombine :: Handle -> Int -> String -> IO Bool
materialSetLayerCombine h i str = let func = {# call unsafe material_set_layer_combine #}
                                  in liftM cToBool $ propagateGError $ \gerrorPtr ->
                                       withHandle h $ \hPtr ->
                                         withCString str $ \strPtr ->
                                           func hPtr (cIntConv i) strPtr (castPtr gerrorPtr)

{# fun unsafe material_set_layer_combine_constant as ^
  { withHandle* `Handle', `Int', withColor* `Color' } -> `()' #}

{# fun unsafe material_set_layer_matrix as ^
  { withHandle* `Handle', `Int', withMatrix* `Matrix' } -> `()' #}


{# fun unsafe material_get_layers as ^ { withHandle* `Handle' } -> `[Handle]' readHandleList* #}

readHandleList :: GList -> IO [Handle]
readHandleList gsl = (readGList gsl :: IO [Ptr Handle]) >>= mapM (newHandle  . castPtr)


{# fun unsafe material_get_n_layers as ^ { withHandle* `Handle' } -> `Int' #}



{# fun unsafe material_set_layer_filters as ^
  { withHandle* `Handle', `Int', cFromEnum `MaterialFilter', cFromEnum `MaterialFilter' } -> `()' #}


{# fun unsafe material_layer_get_type as ^
  { withHandle* `Handle' } -> `MaterialLayerType' cToEnum #}

{# fun unsafe material_layer_get_texture as ^ { withHandle* `Handle' } -> `Handle' newHandle* #}

{# fun unsafe material_layer_get_min_filter as ^
  { withHandle* `Handle' } -> `MaterialFilter' cToEnum #}

{# fun unsafe material_layer_get_mag_filter as ^
  { withHandle* `Handle' } -> `MaterialFilter' cToEnum #}

