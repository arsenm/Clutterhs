-- -*-haskell-*-
--  ClutterGtkScrollable
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

-- | ClutterScrollable — Interface for scrollable actors
module Graphics.UI.Clutter.Gtk.Scrollable (
-- * Description
-- | 'ClutterScrollable' is an interface for scrollable actors,
--   reusing the 'Gtk.Adjustment' objects from GTK+ do drive the
--   scrolling.
--
-- 'ClutterScrollable' is available since Clutter-GTK 0.10
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'GInterface'
-- |           +----'ClutterScrollable'
-- |
-- @

-- * Types
  ClutterScrollable,
  ClutterScrollableClass,

-- * Methods
  clutterScrollableSetAdjustments,
  clutterScrollableGetAdjustments,

-- * Attributes
  clutterScrollableHadjustment,
  clutterScrollableVadjustment
  ) where

import C2HS

import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Abstract.Object
import System.Glib.Attributes
import System.Glib.Properties
{# import Graphics.UI.Clutter.Gtk.Types #}


{# pointer *ClutterActor as ActorPtr foreign -> Actor nocode #}
{# pointer *GtkWidget as WidgetPtr foreign -> Widget nocode #}
{# pointer *GtkAdjustment as AdjustmentPtr foreign -> Adjustment nocode #}


--CHECKME: Maybe these


-- | Sets the horizontal and vertical adjustments used to determine
--   the position of the scrollable actor.
--
-- [@scrollable@] a ClutterScrollable
--
-- [@h_adjust@] a GtkAdjustment
--
-- [@v_adjust@] a GtkAdjustment
--
-- * Since 0.10
--
{# fun unsafe clutter_scrollable_set_adjustments as ^
    `(ClutterScrollableClass scrollable)' =>
    { withClutterScrollableClass* `scrollable',
      withAdjustment* `Adjustment',
      withAdjustment* `Adjustment' } -> `()' #}



-- | Retrieves the horizontal and vertical adjustments used to
--   determine the position of the scrollable actor.
--
-- [@scrollable@] a ClutterScrollable
--
-- [@Returns@] (h_adjust, v_adjust)
--
-- * Since 0.10
--
clutterScrollableGetAdjustments :: (ClutterScrollableClass scrollable) => scrollable
                                -> IO (Adjustment, Adjustment)
clutterScrollableGetAdjustments scrollable =
  withClutterScrollableClass scrollable $ \scrPtr ->
    alloca $ \hPtrPtr ->
      alloca $ \vPtrPtr -> do
       {# call unsafe clutter_scrollable_get_adjustments #} scrPtr hPtrPtr vPtrPtr
       hAdj <- makeNewObject mkAdjustment (peek hPtrPtr)
       vAdj <- makeNewObject mkAdjustment (peek vPtrPtr)
       return (hAdj, vAdj)



--CHECKME: Maybe, reference

clutterScrollableHadjustment :: (ClutterScrollableClass self) => Attr self Adjustment
clutterScrollableHadjustment = newAttrFromObjectProperty "hadjustment" gTypeAdjustment


clutterScrollableVadjustment :: (ClutterScrollableClass self) => Attr self Adjustment
clutterScrollableVadjustment = newAttrFromObjectProperty "vadjustment" gTypeAdjustment

