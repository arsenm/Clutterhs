-- -*-haskell-*-
--  ClutterGstVideoSink
--
--  Author : Matthew Arsenault
--
--  Created: 13 Feb 2010
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

-- | ClutterGstVideoSink — GStreamer video sink
module Graphics.UI.Clutter.Gst.VideoSink (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Gst.Object'
-- |           +----'Gst.Element'
-- |                  +----'Gst.BaseSink.'
-- |                          +----'Clutter.Gst.VideoSink.'
-- @

-- * Types
  VideoSink,
  VideoSinkClass,

-- * Constructors
  videoSinkNew,

-- * Attributes
  videoSinkTexture,
  videoSinkUpdatePriority
  ) where

import C2HS
import System.Glib.Properties
import System.Glib.Attributes

import Graphics.UI.Clutter.Types
import qualified Graphics.UI.Clutter.GTypes as GT

{# import Graphics.UI.Clutter.Gst.Types #}

{# pointer *ClutterTexture as TexturePtr foreign -> Texture nocode #}



-- | Creates a new GStreamer video sink which uses texture as the
-- target for sinking a video stream from GStreamer.
--
-- * Note
--
-- This function has to be called from Clutter's main thread. While
-- GStreamer will spawn threads to do its work, we want all the GL
-- calls to happen in the same thread. Clutter-gst knows which thread
-- it is by assuming this constructor is called from the Clutter
-- thread.
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] a GstElement for the newly created video sink
--
{# fun unsafe video_sink_new as ^
  `(TextureClass texture)' => { withTextureClass* `texture' } -> `VideoSink' newVideoSink* #}



-- CHECKME: Does the type of the texture matter

-- | This is the texture the video is decoded into. It can be any
-- ClutterTexture, however Cluter-Gst has a handy subclass,
-- 'Clutter.Gst.VideoTexture', that implements 'Clutter.MediaClass'.
--
videoSinkTexture :: Attr VideoSink Texture
videoSinkTexture = newAttrFromObjectProperty "texture" GT.texture



-- | Clutter-Gst installs a GSource to signal that a new frame is
-- ready to the Clutter thread. This property allows to tweak the
-- priority of the source (Lower value is higher priority).
--
-- Allowed values: >= -2147483647
--
-- Default value: 100
--
-- * Since 1.0
--
videoSinkUpdatePriority :: Attr VideoSink Int
videoSinkUpdatePriority = newAttrFromIntProperty "update-priority"

