-- -*-haskell-*-
--  ClutterGst Types
--
--  Author : Matthew Arsenault
--
--  Created: 12 Feb 2010
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
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

#include <clutter/clutter.h>
#include <clutter-gst/clutter-gst.h>

{# context lib="clutter_gst" prefix="clutter_gst" #}

module Graphics.UI.Clutter.Gst.Types (
  VideoTexture,
  VideoTextureClass,
  newVideoTexture,
  toVideoTexture,
  withVideoTexture
  ) where

import C2HS
import Graphics.UI.Clutter.Types
import System.Glib.GObject

-- *** VideoTexture

{# pointer *ClutterGstVideoTexture as VideoTexture foreign newtype #}

class (GObjectClass o) => VideoTextureClass o
toVideoTexture :: VideoTextureClass o => o -> VideoTexture
toVideoTexture = unsafeCastGObject . toGObject

--newVideoTexture :: (ActorClass actor) => Ptr actor -> IO VideoTexture
newVideoTexture :: Ptr () -> IO VideoTexture
newVideoTexture a = makeNewActor (VideoTexture, objectUnrefFromMainloop) $ return (castPtr a)


instance ActorClass VideoTexture
instance VideoTextureClass VideoTexture
instance ScriptableClass VideoTexture
instance GObjectClass VideoTexture where
  toGObject (VideoTexture s) = constrGObject (castForeignPtr s)
  unsafeCastGObject (GObject o) = VideoTexture (castForeignPtr o)


