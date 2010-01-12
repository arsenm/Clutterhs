-- -*-haskell-*-
--  ClutterGtkViewport
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
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>

{# context lib="clutter" prefix="gtk" #}

-- | ClutterViewport — A scrollable actor
module Graphics.UI.Clutter.Gtk.Viewport (
-- * Description
-- | The 'Gtk.Adjustment's used to control the horizontal and vertical
--   scrolling can be attached to a 'Gtk.Scrollbar' subclass, like
--   'Gtk.HScrollbar' or 'Gtk.VScrollbar'.
--
-- The 'ClutterViewport' can be used inside any 'Clutter.Container'
-- implementation.
--
-- * 'ClutterViewport' is available since Clutter-GTK 0.10
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'ClutterViewport'
-- |
-- @

-- * Types
  ClutterViewport,

-- * Constructors
  clutterViewportNew,

-- * Methods
  clutterViewportGetOrigin,

-- * Attributes
  clutterViewportChild,
  clutterViewportOrigin
  ) where

import C2HS
import Control.Monad (liftM3)

import Graphics.UI.Gtk.Types
import Graphics.UI.Clutter.Types
import qualified Graphics.UI.Clutter.GTypes as CGT
import System.Glib.Attributes
import System.Glib.Properties

{# import Graphics.UI.Clutter.Gtk.Types #}

{# pointer *GtkAdjustment as AdjustmentPtr foreign -> Adjustment nocode #}

withMaybeAdjustment = maybeWith withAdjustment

-- | Creates a new 'ClutterViewport' with the given adjustments.
--
-- [@h_adjust@] @Just@ horizontal adjustment, or @Nothing@
--
-- [@v_adjust@] @Just@ vertical adjustment, or @Nothing@
--
-- [@z_adjust@] @Just@ zoom adjustment, or @Nothing@
--
-- [@Returns@] the newly created viewport actor
--
-- * Since 0.10
--
clutterViewportNew :: (ActorClass a) => Maybe Adjustment -> Maybe Adjustment -> Maybe Adjustment -> IO (ClutterViewport a)
clutterViewportNew adj1 adj2 adj3 = let func = {# call unsafe clutter_viewport_new #}
                                    in withMaybeAdjustment adj1 $ \adjPtr1 ->
                                         withMaybeAdjustment adj2 $ \adjPtr2 ->
                                           withMaybeAdjustment adj3 $ \adjPtr3 ->
                                               return . mkClutterViewport (undefined :: a)
                                                   =<< newClutterViewportRaw =<< func adjPtr1 adjPtr2 adjPtr3



-- | Retrieves the current translation factor ("origin") used when
--   displaying the child of viewport.
--
-- [@viewport@] : a ClutterViewport
--
-- [@Returns@] (X origin in pixels,
--              Y origin in pixels,
--              Z origin in pixels)
--
-- * Since 0.10
--
clutterViewportGetOrigin :: ClutterViewport a -> IO Vertex
clutterViewportGetOrigin viewport = let func = {# call unsafe clutter_viewport_get_origin #}
                                        in withClutterViewport viewport $ \viewPtr ->
                                             alloca $ \xPtr ->
                                               alloca $ \yPtr ->
                                                 alloca $ \zPtr ->
                                                   func viewPtr xPtr yPtr zPtr
                                                    >> liftM3 Vertex (peekFloatConv xPtr)
                                                                     (peekFloatConv yPtr)
                                                                     (peekFloatConv zPtr)


-- | The 'Actor' inside the viewport.
--
-- * Since 0.10
--
clutterViewportChild :: (ActorClass a) => Attr (ClutterViewport a) a
clutterViewportChild = newAttrFromObjectProperty "child" CGT.actor


-- | The current origin of the viewport. You should use the vertex to
--   convert event coordinates for the child of the viewport.
--
-- * Since 0.10
--
clutterViewportOrigin :: (ActorClass a) => Attr (ClutterViewport a) Vertex
clutterViewportOrigin = newAttrFromBoxedStorableProperty "origin" CGT.vertex


