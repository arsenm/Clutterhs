-- -*-haskell-*-
--  ClutterGtk Types
--
--  Author : Matthew Arsenault
--
--  Created: 13 Nov 2009
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

#include <clutter-gtk/clutter-gtk.h>

{# context lib="clutter" prefix="gtk" #}

module Graphics.UI.Clutter.Gtk.Types (
  ClutterEmbed,
  ClutterEmbedClass,
  newClutterEmbed,
  toClutterEmbed,
  withClutterEmbed
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Graphics.UI.Gtk.Types
import System.Glib.GObject

{# pointer *GtkClutterEmbed as ClutterEmbed foreign newtype #}

--CHECKME: Check these constriants
class (ObjectClass o, GObjectClass o) => ClutterEmbedClass o
toClutterEmbed :: ClutterEmbedClass o => o -> ClutterEmbed
toClutterEmbed = unsafeCastGObject . toGObject

newClutterEmbed :: (WidgetClass actor) => Ptr actor -> IO ClutterEmbed
newClutterEmbed a = makeNewActor (ClutterEmbed, objectUnref) $ return (castPtr a)

instance ClutterEmbedClass ClutterEmbed
instance WidgetClass ClutterEmbed
instance ObjectClass ClutterEmbed
instance GObjectClass ClutterEmbed where
  toGObject (ClutterEmbed s) = constrGObject (castForeignPtr s)
  unsafeCastGObject (GObject o) = ClutterEmbed (castForeignPtr o)

