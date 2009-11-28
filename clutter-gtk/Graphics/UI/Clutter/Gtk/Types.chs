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
  withClutterEmbed,

  ClutterScrollable,
  ClutterScrollableClass,
  withClutterScrollableClass,
  withClutterScrollable,
  toClutterScrollable,

  ClutterZoomable,
  ClutterZoomableClass,
  withClutterZoomableClass,
  withClutterZoomable,
  toClutterZoomable,


  newAdjustment,
  withAdjustment
  ) where


import C2HS
import Graphics.UI.Clutter.Types
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Abstract.Object
import System.Glib.GObject

-- *** Embed

{# pointer *GtkClutterEmbed as ClutterEmbed foreign newtype #}

--CHECKME: Check these constriants
class (ObjectClass o, GObjectClass o) => ClutterEmbedClass o
toClutterEmbed :: ClutterEmbedClass o => o -> ClutterEmbed
toClutterEmbed = unsafeCastGObject . toGObject

newClutterEmbed :: (WidgetClass widget) => Ptr widget -> IO ClutterEmbed
newClutterEmbed a = makeNewObject (ClutterEmbed, objectUnrefFromMainloop) $ return (castPtr a)

--CHECKME:
-- GtkClutterEmbed implements AtkImplementorIface and GtkBuildable.


instance ClutterEmbedClass ClutterEmbed
instance WidgetClass ClutterEmbed
instance ObjectClass ClutterEmbed
instance GObjectClass ClutterEmbed where
  toGObject (ClutterEmbed s) = constrGObject (castForeignPtr s)
  unsafeCastGObject (GObject o) = ClutterEmbed (castForeignPtr o)



-- *** Scrollable

{# pointer *GtkClutterScrollable as ClutterScrollable foreign newtype #}

class (GObjectClass o) => ClutterScrollableClass o
toClutterScrollable :: ClutterScrollableClass o => o -> ClutterScrollable
toClutterScrollable = unsafeCastGObject . toGObject

withClutterScrollableClass :: ClutterScrollableClass o => o -> (Ptr ClutterScrollable -> IO a) -> IO a
withClutterScrollableClass = withClutterScrollable . toClutterScrollable


--CHECKME:
-- GtkClutterEmbed implements AtkImplementorIface and GtkBuildable.


instance ClutterScrollableClass ClutterScrollable
instance GObjectClass ClutterScrollable where
  toGObject (ClutterScrollable s) = constrGObject (castForeignPtr s)
  unsafeCastGObject (GObject o) = ClutterScrollable (castForeignPtr o)


-- *** Zoomable

{# pointer *GtkClutterZoomable as ClutterZoomable foreign newtype #}

class (GObjectClass o) => ClutterZoomableClass o
toClutterZoomable :: ClutterZoomableClass o => o -> ClutterZoomable
toClutterZoomable = unsafeCastGObject . toGObject

withClutterZoomableClass :: ClutterZoomableClass o => o -> (Ptr ClutterZoomable -> IO a) -> IO a
withClutterZoomableClass = withClutterZoomable . toClutterZoomable

instance ClutterZoomableClass ClutterZoomable
instance GObjectClass ClutterZoomable where
  toGObject (ClutterZoomable s) = constrGObject (castForeignPtr s)
  unsafeCastGObject (GObject o) = ClutterZoomable (castForeignPtr o)

-- * Misc

--used by zoomable and scrollable

newAdjustment :: Ptr Adjustment -> IO Adjustment
newAdjustment a = makeNewObject mkAdjustment $ return (castPtr a)


withAdjustment :: Adjustment -> (Ptr Adjustment -> IO a) -> IO a
withAdjustment = withForeignPtr . unAdjustment


