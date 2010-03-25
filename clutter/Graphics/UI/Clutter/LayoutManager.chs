-- -*-haskell-*-
--  Clutter LayoutManager
--
--  Author : Matthew Arsenault
--
--  Created: 14 Mar 2010
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

{# context lib="clutter" prefix="clutter" #}


-- | LayoutManager — Layout managers base class
module Graphics.UI.Clutter.LayoutManager (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- * Description
-- | 'LayoutManager' is a base abstract class for layout managers. A
-- layout manager implements the layouting policy for a composite or a
-- container actor: it controls the preferred size of the actor to
-- which it has been paired, and it controls the allocation of its
-- children.
--
-- Any composite or container 'Actor' subclass can delegate the
-- layouting of its children to a 'LayoutManager'. Clutter provides a
-- generic container using 'LayoutManager' called 'Box'.
--
-- Clutter provides some simple 'LayoutManager' sub-classes, like
-- 'FlowLayout' and 'BinLayout'.
--

-- * Class Hierarchy
-- |
-- @
-- |    'GObject'
-- |     +----GInitiallyUnowned
-- |           +----'LayoutManager'
-- |                  +----'FixedLayout'
-- |                  +----'BinLayout'
-- |                  +----'FlowLayout'
-- |                  +----'BoxLayout'
-- @
--

-- * Types
  LayoutManager,
  LayoutManagerClass,

-- * Constructors

-- * Methods
-- * Attributes
-- * Signals
#endif
  ) where

#if CLUTTER_CHECK_VERSION(1,2,0)

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes




-- | Computes the minimum and natural widths of the container
-- according to manager.
--
-- See also 'actorGetPreferredWidth'
--
-- [@manager@] a 'LayoutManager'
--
-- [@container@] the 'Container' using manager
--
-- [@for_height@] the height for which the width should be computed, or -1
--
-- [@Returns@] (min_width_p, nat_width_p)
--
-- * Since 1.2
--
{# fun unsafe layout_manager_get_preferred_width as ^
  `(LayoutManagerClass manager, ContainerClass container)' =>
    { withLayoutManager* `manager', withContainerClass* `container', `Float', alloca- `Float', alloca- `Float' } -> `()'  #}



-- | Computes the minimum and natural heights of the container
-- according to manager.
--
-- See also 'actorGetPreferredHeight'
--
-- [@manager@] a 'LayoutManager'
--
-- [@container@] the 'Container' using manager
--
-- [@for_width@] the width for which the height should be computed, or
-- -1
--
-- [@Returns@] (min_height_p, nat_height_p)
--
-- * Since 1.2
--

{# fun unsafe layout_manager_get_preferred_height as ^
  `(LayoutManagerClass manager, ContainerClass container)' =>
    { withLayoutManager* `manager', withContainerClass* `container', `Float', alloca- `Float', alloca- `Float' } -> `()'  #}




-- | Allocates the children of container given an area
--
-- See also 'actorAllocate'
--
-- [@manager@] a 'LayoutManager'
--
-- [@container@] the 'Container' using manager
--
-- [@allocation@] the 'ActorBox' containing the allocated area of
-- container
--
-- [@flags@] the allocation flags
--
-- * Since 1.2
--
{# fun unsafe layout_manager_allocate as ^ `(LayoutManagerClass manager, ContainerClass container)' =>
  { withLayoutManagerClass* `manager',
    withContainerClass* `container',
    withActorBox* `ActorBox',
    cFromFlags `[AllocationFlags]' } -> `()' #}



-- | Emits the 'layoutChanged' signal on manager
--
-- This function should only be called by implementations of the
-- 'LayoutManager' class
--
-- [@manager@] a 'LayoutManager'
--
-- * Since 1.2
--
{# fun layout_manager_layout_changed as ^ `(LayoutManagerClass manager)' =>
  { withManagerClass* `manager' } -> `()' #}


--CHECKME: What is this used for / I'm not sure to keep this. I'm
--following the rule if I'm not sure what to do with it, leave it.

-- | If the 'LayoutManager' sub-class allows it, allow adding a weak
-- reference of the container using manager from within the layout
-- manager
--
-- The layout manager should not increase the reference count of the
-- container
--
-- [@manager@] a 'LayoutManager'
--
-- [@container@] a 'Container' using manager.
--
-- * Since 1.2
--
{# fun unsafe layout_manager_set_container as ^
  `(LayoutManagerClass manager, ContainerClass container)' =>
    { withManagerClass* `manager', withContainerClass* `container' } -> `()' #}



-- {# fun unsafe layout_manager_get_child_meta
-- {# fun unsafe layout_manager_child_set
-- {# fun unsafe layout_manager_child_set_property
-- {# fun unsafe layout_manager_child_get
-- {# fun unsafe layout_manager_child_get_property
-- {# fun unsafe layout_manager_find_child_property
-- {# fun unsafe layout_manager_list_child_properties




-- | Begins an animation of duration milliseconds, using the provided
-- easing mode
--
-- The easing mode can be specified either as an 'AnimationMode' or as
-- a logical id returned by 'alphaRegisterFunc'
--
-- The result of this function depends on the manager implementation
--
-- [@manager@] a 'LayoutManager'
--
-- [@duration@] the duration of the animation, in milliseconds
--
-- [@mode@] the easing mode of the animation
--
-- [@Returns@] transfer none. transfer none.
--
-- * Since 1.2
--
{# fun unsafe layout_manager_begin_animation as ^ `(LayoutManager manager)' =>
  { withLayoutManagerClass* `manager', `Word', `Word' } -> `Alpha' newAlpha* #}


-- | Ends an animation started by 'layoutManagerBeginAnimation'
--
-- The result of this call depends on the manager implementation
--
-- [@manager@] a 'LayoutManager'
--
-- * Since 1.2
--
{# fun unsafe layout_manager_end_animation as ^ `(LayoutManager manager)' =>
  { withLayoutManagerClass* `manager' } -> `()' #}


-- | Retrieves the progress of the animation, if one has been started
-- by 'layoutManagerBeginAnimation'
--
-- The returned value has the same semantics of the "alpha" value
--
-- [@manager@] a 'LayoutManager'
--
-- [@Returns@] the progress of the animation
--
-- * Since 1.2
--
{# fun unsafe layout_manager_get_animation_progress as ^ `(LayoutManager manager)' =>
  { withLayoutManagerClass* `manager' } -> `Double' #}


-- | The ::'layoutChanged' signal is emitted each time a layout
-- manager has been changed. Every ClutterActor using the manager
-- instance as a layout manager should connect a handler to the
-- ::'layoutChanged' signal and queue a relayout on themselves:
--
-- * Since 1.2
--
layoutChanged :: (LayoutManagerClass manager) => Signal manager (IO ())
layoutChanged = Signal (connect_NONE__NONE "layout-changed")

#endif

