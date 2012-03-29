-- -*-haskell-*-
--  ClutterGtkZoomable
--
--  Author : Matthew Arsenault
--
--  Created: 28 Nov 2009
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
#include <clutter-gtk/clutter-gtk.h>

{# context lib="clutter" prefix="gtk" #}

-- | ClutterZoomable — Interface for zoomable actors
module Graphics.UI.Clutter.Gtk.Zoomable (
-- * Description
-- | 'ClutterZoomable' is an interface for zoomable actors, using,
--   like 'ClutterScrollable', the 'Gtk.Adjustment' objects from GTK+
--   to drive the zoom factor.
--
-- * ClutterZoomable is available since Clutter-GTK 1.0
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'GInterface'
-- |           +----'ClutterZoomable'
-- |
-- @

-- * Types
  ClutterZoomable,
  ClutterZoomableClass,

-- * Methods
  clutterZoomableSetAdjustment,
  clutterZoomableGetAdjustment
  ) where

import C2HS

import Graphics.UI.GtkInternals

{# import Graphics.UI.Clutter.Gtk.Types #}


{# pointer *GtkAdjustment as AdjustmentPtr foreign -> Adjustment nocode #}


--CHECKME: Maybe these


-- | Sets the adjustment used to determine the zoom factor of the
--   zoomable actor
--
-- [@zoomable@] a ClutterZoomable
--
-- [@z_adjust@] a 'Gtk.Adjustment'
--
-- * Since 0.10
--
{# fun unsafe clutter_zoomable_set_adjustment as ^
    `(ClutterZoomableClass scrollable)' =>
    { withClutterZoomableClass* `scrollable', withAdjustment* `Adjustment' } -> `()' #}


-- | Retrieves the adjustment used to determine the zoom factor of the
--   zoomable actor
--
-- [@zoomable@] a 'ClutterZoomable'
--
-- [@Returns@] a 'Gtk.Adjustment'
--
-- * Since 0.10
--
{# fun unsafe clutter_zoomable_get_adjustment as ^
    `(ClutterZoomableClass scrollable)' =>
    { withClutterZoomableClass* `scrollable' } -> `Adjustment' newAdjustment* #}


