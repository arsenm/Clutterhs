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

#if CLUTTER_GST_CHECK_VERSION(1,0,0)
  videoTextureGetPipeline,
#endif

-- * Attributes

-- * Signals

-- * Events

  ) where

import C2HS


{# import Graphics.UI.Clutter.Gst.Types #}




-- | Creates a video texture.
--
-- * Note
--
-- This function has to be called from Clutter's main thread. While
-- GStreamer will spawn threads to do its work, we want all the GL
-- calls to happen in the same thread. Clutter-gst knows which thread
-- it is by assuming this constructor is called from the Clutter
-- thread.
--
-- [@Returns@] the newly created video texture actor
--
{# fun unsafe video_texture_new as ^ { } -> `VideoTexture' newVideoTexture* #}



#if CLUTTER_GST_CHECK_VERSION(1,0,0)

-- | Retrieves the 'Gst.Pipeline' used by the texture, for direct use
-- with GStreamer API.
--
-- [@texture@] a 'VideoTexture'
--
-- [@Returns@] the pipeline element used by the video texture
--
{# fun unsafe video_texture_get_pipeline as ^
  { withVideoTexture* `VideoTexture' } -> `Pipeline' newPipeline* #}

#endif


