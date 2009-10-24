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

                                  --textureSetFromFile,
                                  --textureSetFromRgbData,  --TODO: RGB??? vs. Rgb
                                  --textureSetFromYuvData,
                                  --textureSetAreaFromRgbData,
                                    textureGetBaseSize,
                                    textureBaseSize,

                                  --textureGetPixelFormat,
                                  --textureGetMaxTileWaste,

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
import Control.Monad (liftM, unless, when)
import System.Glib.Attributes
import System.Glib.GError
import System.IO (hPutStrLn, stderr)


--allocaGErrorAddr act = alloca $ \gep -> do
--                       sptr <- newStablePtr gep

{# fun unsafe texture_new as ^ { } -> `Texture' newTexture* #}

--{# fun unsafe texture_new_from_file as ^ { `String', alloca- `GError' peek* } -> `Texture' newTexture* #}

textureNewFromFile :: String -> IO Texture
textureNewFromFile filename = withCString filename $ \cstr -> do
                              txt <- propagateGError $ \gerrorPtr ->
                                                          {# call unsafe texture_new_from_file #} cstr (castPtr gerrorPtr)
                              newTexture txt
{-
                             --FIXME: Make this a better message. use with checkGError
                              (\err@(GError domain code msg) -> hPutStrLn stderr
                               ("textureNewFromFile: Error: " ++ show domain ++ " " ++ show code ++ " " ++ show msg)
                              >> return Prelude.Nothing)
 -}
                              --somehowthe thing isn't happening

{# fun unsafe texture_new_from_actor as ^
       `(ActorClass a)' => { withActorClass* `a'} -> `Texture' newTexture* #}

{# fun unsafe texture_get_base_size as ^
       { withTexture* `Texture',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}
textureBaseSize :: ReadAttr Texture (Int, Int)
textureBaseSize = readAttr textureGetBaseSize


{# fun unsafe texture_get_filter_quality as ^
       { withTexture* `Texture' } -> `TextureQuality' cToEnum #}
{# fun unsafe texture_set_filter_quality as ^
       { withTexture* `Texture', cFromEnum `TextureQuality' } -> `()' #}
textureFilterQuality :: Attr Texture TextureQuality
textureFilterQuality = newAttr textureGetFilterQuality textureSetFilterQuality




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

