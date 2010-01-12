-- -*-haskell-*-
--  Clutter Gtk
--
--  Author : Matthew Arsenault
--
--  Created: 13 Nov 2009
--
--  Copyright (C) 2009-2010 Matthew Arsenault
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

-- | Clutter-Gtk is an integration library for the Clutter toolkit
--   providing access and integration for the GTK+ library.
--
--
-- Clutter-Gtk allows embedding a Clutter scene graph into a GTK+
-- application; retrieving themeing information; integrate GTK+ data
-- structures and object classes with Clutter actors.
--
module Graphics.UI.Clutter.Gtk (
  module Graphics.UI.Clutter.Gtk.Embed,
  module Graphics.UI.Clutter.Gtk.Scrollable,
  module Graphics.UI.Clutter.Gtk.Zoomable,
  module Graphics.UI.Clutter.Gtk.Viewport,
  module Graphics.UI.Clutter.Gtk.Utility
  ) where

import Graphics.UI.Clutter.Gtk.Embed
import Graphics.UI.Clutter.Gtk.Scrollable
import Graphics.UI.Clutter.Gtk.Zoomable
import Graphics.UI.Clutter.Gtk.Viewport
import Graphics.UI.Clutter.Gtk.Utility

