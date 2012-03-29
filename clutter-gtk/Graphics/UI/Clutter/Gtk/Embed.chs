-- -*-haskell-*-
--  ClutterGtkEmbed
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

#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>

{# context lib="clutter" prefix="gtk" #}

--TODO: How should these be prefixed? Get rid of clutterpart, or gtk,
--or both? Also the type.

-- | GtkEmbed — Widget for embedding a Clutter scene
module Graphics.UI.Clutter.Gtk.Embed (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Gtk.Object'
-- |           +----'Gtk.Widget'
-- |                  +----'Gtk.Container'
-- |                        +----'ClutterEmbed'
-- |
-- @

-- * Types
  ClutterEmbed,
  ClutterEmbedClass,

-- * Constructors
  clutterEmbedNew,

-- * Methods
  clutterEmbedGetStage
  ) where

import C2HS

import Graphics.UI.Clutter.Types
import Graphics.UI.GtkInternals
{# import Graphics.UI.Clutter.Gtk.Types #}


{# pointer *ClutterActor as ActorPtr foreign -> Actor nocode #}
{# pointer *GtkWidget as WidgetPtr foreign -> Widget nocode #}


-- | Creates a new 'ClutterEmbed' widget. This widget can be used to build
--   a scene using Clutter API into a GTK+ application.
--
-- [@Returns@] the newly created 'Embed'
--
-- * Since 0.6
--
{# fun unsafe clutter_embed_new as ^ { } -> `ClutterEmbed' newClutterEmbed* #}

-- CHECKME: You should never destroy or unref
-- | Retrieves the 'Stage' from embed. The returned stage can be used
--   to add actors to the Clutter scene.
--
-- [@embed@] a 'ClutterEmbed'
--
-- [@Returns@] the Clutter stage.
--
-- * Since 0.6
--
{# fun unsafe clutter_embed_get_stage as ^
    { withClutterEmbed* `ClutterEmbed' } -> `Stage' newStage* #}


