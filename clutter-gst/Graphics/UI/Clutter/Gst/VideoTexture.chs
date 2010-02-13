-- -*-haskell-*-
--  ClutterGstVideoTexture
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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>
#include <clutter-gst/clutter-gst.h>

{# context lib="clutter_gst" prefix="clutter_gst" #}


-- | ClutterGstVideoTexture — Actor for playback of video files.
module Graphics.UI.Clutter.Gst.VideoTexture (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'Texture'
-- |                  +----'VideoTexture'
-- @

-- * Types
  VideoTexture,
  VideoTextureClass,

-- * Constructors
  videoTextureNew

-- * Methods

-- * Attributes

-- * Signals

-- * Events

  ) where

import C2HS
import Graphics.UI.Clutter.Gst.Types

{# fun unsafe video_texture_new as ^ { } -> `VideoTexture' newVideoTexture* #}


