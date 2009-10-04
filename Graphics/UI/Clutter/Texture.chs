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

                                    textureSetKeepAspectRatio,
                                    textureGetKeepAspectRatio,
                                    textureKeepAspectRatio
                                   ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM, unless, when)
import System.Glib.Attributes
import System.Glib.GError
import System.IO (hPutStrLn, stderr)


--allocaGErrorAddr act = alloca $ \gep -> do
--                       sptr <- newStablePtr gep

{# fun unsafe texture_new as ^ {} -> `Texture' newTexture* #}

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

{# fun unsafe texture_new_from_actor as ^ `(ActorClass a)' => { withActorClass* `a'} -> `Texture' newTexture* #}

{# fun unsafe texture_get_keep_aspect_ratio as ^ { withTexture* `Texture' } -> `Bool' #}
{# fun unsafe texture_set_keep_aspect_ratio as ^ { withTexture* `Texture', `Bool'} -> `()' #}
textureKeepAspectRatio :: Attr Texture Bool
textureKeepAspectRatio = newAttr textureGetKeepAspectRatio textureSetKeepAspectRatio

