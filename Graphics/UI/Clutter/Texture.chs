-- -*-haskell-*-
--  Clutter Texture
--
--  Author : Matthew Arsenault
--
--  Created: 3 Oct 2009
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

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Texture (
                                    textureNew,
                                    textureNewFromFile,
                                    textureNewFromActor,

                                    textureSetFromFile,
                                  --textureSetFromRgbData,  --TODO: RGB??? vs. Rgb
                                  --textureSetFromYuvData,
                                  --textureSetAreaFromRgbData,
                                    textureGetBaseSize,
                                    textureBaseSize,

                                  --textureGetPixelFormat,
                                    textureGetMaxTileWaste,

                                    textureGetFilterQuality,
                                    textureSetFilterQuality,
                                    textureFilterQuality,

                                  --textureGetCoglTexture,
                                  --textureSetCoglTexture,
                                  --textureCoglTexture,

                                  --textureSetCoglMaterial,
                                  --textureGetCoglMaterial,
                                  --textureCoglMaterial,

                                    textureGetSyncSize,
                                    textureSetSyncSize,
                                    textureSyncSize,

                                    textureGetRepeat,
                                    textureSetRepeat,
                                    textureRepeat,

                                    textureSetKeepAspectRatio,
                                    textureGetKeepAspectRatio,
                                    textureKeepAspectRatio,

                                    textureGetLoadAsync,
                                    textureSetLoadAsync,
                                    textureLoadAsync,

                                    textureGetLoadDataAsync,
                                    textureSetLoadDataAsync,
                                    textureLoadDataAsync
                                   ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.GError

{# fun unsafe texture_new as ^ { } -> `Texture' newTexture* #}

textureNewFromFile :: String -> IO Texture
textureNewFromFile filename = let func = {# call unsafe texture_new_from_file #}
                              in withCString filename $ \cstr ->
                                   propagateGError $ \gerrorPtr ->
                                       func cstr (castPtr gerrorPtr) >>= newTexture

{# fun unsafe texture_new_from_actor as ^
       `(ActorClass a)' => { withActorClass* `a'} -> `Texture' newTexture* #}

textureSetFromFile :: Texture -> String -> IO Bool
textureSetFromFile txt fname = let func = {# call unsafe texture_set_from_file #}
                               in liftM cToBool $ withTexture txt $ \txtptr ->
                                    withCString fname $ \cstr ->
                                      propagateGError $ \gerrorPtr ->
                                        func txtptr cstr (castPtr gerrorPtr)
{-
--CHECKME: Rgb or RGB?
--Um..how to deal with random pointer? Not possible?
textureSetFromRgbData :: Texture ->
                         ?? ->
                         Bool ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         [TextureFlags] ->
                         IO Bool
textureSetFromRgbData txt dat hasA width height rowstride bpp flags =
    let func = {# call unsafe texture_set_from_rgb_data #}
    in withTexture txt $ \txtptr ->
        propagateGError $ \gerrorPtr ->
            func txtptr
                 dat
                 (cFromBool hasA)
                 (cIntConv width)
                 (cIntConv height)
                 (cIntConv rowstride)
                 (cIntConv dpp)
                 (cFromFlags flags)
                 (castPtr gerrorPtr)

tetxure_set_from_yuv_data
tetxure_set_area_from_rgb_data
-}


{# fun unsafe texture_get_base_size as ^
       { withTexture* `Texture',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}
textureBaseSize :: ReadAttr Texture (Int, Int)
textureBaseSize = readAttr textureGetBaseSize

{- TODO: Requires cogl
{# fun unsafe texture_get_pixel_format as ^
       { withTexture* `Texture' } -> `CoglPixelFormat' cToEnum #}
-}

{# fun unsafe texture_get_max_tile_waste as ^
       { withTexture* `Texture' } -> `Int' #}


{# fun unsafe texture_get_filter_quality as ^
       { withTexture* `Texture' } -> `TextureQuality' cToEnum #}
{# fun unsafe texture_set_filter_quality as ^
       { withTexture* `Texture', cFromEnum `TextureQuality' } -> `()' #}
textureFilterQuality :: Attr Texture TextureQuality
textureFilterQuality = newAttr textureGetFilterQuality textureSetFilterQuality

{-
{# fun unsafe texture_get_cogl_texture as ^
       { withTexture* `Texture' } -> `CoglTexture' newCoglTexture #}
{# fun unsafe texture_set_cogl_texture as ^
       { withTexture* `Texture', withCoglTexture `CoglTexture' } -> `()' #}
textureCoglTexture :: Attr Texture CoglTexture
textureCoglTexture = newAttr textureGetCoglTexture textureSetCoglTexture

{# fun unsafe texture_get_cogl_material as ^
       { withTexture* `Texture' } -> `CoglMaterial' newCoglMaterial #}
{# fun unsafe texture_set_cogl_material as ^
       { withTexture* `Texture', withCoglMaterial `CoglMaterial' } -> `()' #}
textureCoglMaterial :: Attr Texture CoglMaterial
textureCoglMaterial = newAttr textureGetCoglMaterial textureSetCoglMaterial
-}

{# fun unsafe texture_get_sync_size as ^ { withTexture* `Texture' } -> `Bool' #}
{# fun unsafe texture_set_sync_size as ^ { withTexture* `Texture', `Bool' } -> `()' #}
textureSyncSize :: Attr Texture Bool
textureSyncSize = newAttr textureGetSyncSize textureSetSyncSize

{# fun unsafe texture_get_repeat as ^
       { withTexture* `Texture', alloca- `Bool' peekBool*, alloca- `Bool' peekBool* } -> `()' #}
{# fun unsafe texture_set_repeat as ^ { withTexture* `Texture', `Bool', `Bool' } -> `()' #}
textureRepeat :: Attr Texture (Bool, Bool)
textureRepeat = newAttr textureGetRepeat (tup2ToF textureSetRepeat)


{# fun unsafe texture_get_keep_aspect_ratio as ^ { withTexture* `Texture' } -> `Bool' #}
{# fun unsafe texture_set_keep_aspect_ratio as ^ { withTexture* `Texture', `Bool'} -> `()' #}
textureKeepAspectRatio :: Attr Texture Bool
textureKeepAspectRatio = newAttr textureGetKeepAspectRatio textureSetKeepAspectRatio


{# fun unsafe texture_get_load_async as ^ { withTexture* `Texture' } -> `Bool' #}
{# fun unsafe texture_set_load_async as ^ { withTexture* `Texture', `Bool'} -> `()' #}
textureLoadAsync :: Attr Texture Bool
textureLoadAsync = newAttr textureGetLoadAsync textureSetLoadAsync


{# fun unsafe texture_get_load_data_async as ^ { withTexture* `Texture' } -> `Bool' #}
{# fun unsafe texture_set_load_data_async as ^ { withTexture* `Texture', `Bool'} -> `()' #}
textureLoadDataAsync :: Attr Texture Bool
textureLoadDataAsync = newAttr textureGetLoadDataAsync textureSetLoadDataAsync

