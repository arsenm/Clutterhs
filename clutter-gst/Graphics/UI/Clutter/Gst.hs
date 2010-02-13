-- -*-haskell-*-
--  Clutter Gst
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

-- | Clutter-GStreamer provides a ClutterMedia interface
-- implementation using GStreamer for basic video and audio playback.
--
module Graphics.UI.Clutter.Gst (
  module Graphics.UI.Clutter.Gst.VideoTexture,
  module Graphics.UI.Clutter.Gst.VideoSink,
  module Graphics.UI.Clutter.Gst.Utilities
  ) where

import Graphics.UI.Clutter.Gst.VideoTexture
import Graphics.UI.Clutter.Gst.VideoSink
import Graphics.UI.Clutter.Gst.Utilities

