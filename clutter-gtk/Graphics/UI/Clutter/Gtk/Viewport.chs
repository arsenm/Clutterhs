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
{-# LANGUAGE ForeignFunctionInterface #-}

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
---
-- @

-- * Types
  ClutterViewport,
  ClutterViewportClass,

-- * Constructors
  clutterViewportNew,

-- * Methods
  clutterViewportGetOrigin,

-- * Attributes
  clutterViewportChild,
  clutterViewportOrigin
  ) where

import C2HS

import Graphics.UI.Gtk.Types
import Graphics.UI.Clutter.Types

{# import Graphics.UI.Clutter.Gtk.Types #}


--CHECKME: Maybe these


-- | Creates a new 'ClutterViewport' with the given adjustments.
--
-- [@h_adjust@] horizontal adjustment, or NULL
--
-- [@v_adjust@] vertical adjustment, or NULL
--
-- [@z_adjust@] zoom adjustment, or NULL
--
-- [@Returns@] the newly created viewport actor
--
-- * Since 0.10
--
{# fun unsafe clutter_viewport_new as ^
   `(ActorClass a)' =>
    { withAdjustment* `Adjustment',
      withAdjustment* `Adjustment',
      withAdjustment* `Adjustment' } -> `Viewport a' #}
clutterViewportNew :: (ActorClass a) => Adjustment -> Adjustment -> Adjustment -> IO (Viewport a)
clutterViewportNew adj1 adj2 adj3 = let func = {# call unsafe clutter_viewport_new #}
                                    in withAdjustment adj1 $ \adjPtr1 ->
                                         withAdjustment adj3 $ \adjPtr2 ->
                                           withAdjustment adj3 $ \adjPtr3 ->
                                           raw <- newViewportRaw =<< func adjPtr1 adjPtr2 adjPtr3




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
clutterViewportGetOrigin :: Viewport -> IO Vertex
clutterViewportGetOrigin viewport = let func = {# call unsafe clutter_viewport_get_origin #}
                                        in withViewport viewport $ \viewPtr ->
                                             alloca $ \xPtr ->
                                               alloca $ \yPtr ->
                                                 alloca $ \zPtr -> do
                                                     func xPtr yPtr zPtr
                                                     x <- peekFloatConv x
                                                     y <- peekFloatConv y
                                                     z <- peekFloatConv z
                                                     return (Vertex x y z)




--TODO: Wrapper type for type of child

-- | The 'Actor' inside the viewport.
--
-- * Since 0.10
--
clutterViewportChild :: Attr Viewport Actor
clutterViewportChild = newAttrFromObjectProperty "child" gTypeActor


-- | The current origin of the viewport. You should use the vertex to
--   convert event coordinates for the child of the viewport.
--
-- * Since 0.10
--
clutterViewportOrigin :: Attr Viewport Vertex
clutterViewportOrigin = newAttrFromBoxedStorableProperty "origin" gTypeVertex


