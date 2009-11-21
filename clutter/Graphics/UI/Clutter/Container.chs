-- -*-haskell-*-
--  Clutter Container
--
--  Author : Matthew Arsenault
--
--  Created: 12 Sep 2009
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

--TODO: Property list of children?

-- | ClutterContainer — An interface for implementing container actors
module Graphics.UI.Clutter.Container (
-- * Class Hierarchy
-- |
-- @
-- |  'GInterface'
-- |   +----'Container'
-- @

-- * Methods
  containerAddActor,
--containerAddList
  containerRemoveActor,
--containerRemoveList,
  containerGetChildren,

  containerForeach,
  containerForeachWithInternals,
  containerFindChildByName,
  containerRaiseChild,
  containerLowerChild,
  containerSortDepthOrder,
--containerClassFindChildProperty,
--containerClassListChildProperties,
--containerChildSetProperty,
--containerChildGetProperty,
--containerChildSet,
--containerChildGet,
  containerGetChildMeta
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject

--CHECKME: unsafe
-- | Adds an Actor to container. This function will emit the "actor-added" signal.
--   The actor should be parented to container. You cannot add a 'Actor' to more
--   than one 'Container'.
{# fun unsafe container_add_actor as ^
       `(ContainerClass container, ActorClass actor)' =>
           { withContainerClass* `container', withActorClass* `actor' } -> `()' #}
--TODO: add/removeList as var args function, to allow multiple types of the actor class in the list

--TODO: Does this doc make sense?
-- | Removes actor from container. The
--   actor should be unparented When the actor has been removed, the
--  "actor-removed" signal is emitted by container.
{# fun unsafe container_remove_actor as ^
       `(ContainerClass container, ActorClass actor)' =>
           { withContainerClass* `container', withActorClass* `actor' } -> `()' #}

-- | Retrieves all the children of container.
{# fun unsafe container_get_children as ^
       `(ContainerClass container)' => { withContainerClass* `container' } -> `[Actor]' newActorList* #}

-- | Calls callback for each child of container that was added by the
--  application (with 'containerAddActor'). Does not iterate
--  over "internal" children that are part of the container's own
--  implementation, if any.
containerForeach :: (ContainerClass c) => c -> Callback -> IO ()
containerForeach c func = withContainerClass c $ \cptr -> do
                            funcPtr <- newCallback func
                            {# call container_foreach #} cptr funcPtr nullPtr
                            freeHaskellFunPtr funcPtr
                            --CHECKME: unsafe?

-- | Calls callback for each child of container, including "internal"
--   children built in to the container itself that were never added by
--   the application.
containerForeachWithInternals :: (ContainerClass c) => c -> Callback -> IO ()
containerForeachWithInternals c func = withContainerClass c $ \cptr -> do
                                         funcPtr <- newCallback func
                                         {# call container_foreach #} cptr funcPtr nullPtr
                                         freeHaskellFunPtr funcPtr
                                       --CHECKME: unsafe?


-- | Finds a child actor of a container by its name. Search recurses into any child container.
{# fun unsafe container_find_child_by_name as ^
       `(ContainerClass container)' => { withContainerClass* `container', `String' } -> `Actor' newActor* #}

-- | Raises actor to sibling level, in the depth ordering.
{# fun unsafe container_raise_child as ^
       `(ContainerClass container, ActorClass actor, ActorClass sibling)' =>
           { withContainerClass* `container',
             withActorClass* `actor',
             withActorClass* `sibling' } -> `()' #}

-- | Lowers actor to sibling level, in the depth ordering.
{# fun unsafe container_lower_child as ^
       `(ContainerClass container, ActorClass actor, ActorClass sibling)' =>
           { withContainerClass* `container',
             withActorClass* `actor',
             withActorClass* `sibling' } -> `()' #}

-- | Sorts a container's children using their depth. This function should not be normally used by applications.
{# fun unsafe container_sort_depth_order as ^
       `(ContainerClass container)' => { withContainerClass* `container' } -> `()' #}
{-
{# fun unsafe container_class_find_child_property as ^
       `(GObjectClass gobj)' => { withGObjectClass* `gobj', `String' } -> `GParamSpec' #}

{# fun unsafe container_class_list_child_properties as ^
       `(GObjectClass gobj)' => { withGObjectClass* `gobj', `Word' } -> `[GParamSpec]' #}
-}
--container_child_set_property
--container_child_get_property
--TODO: Set property functions, hiding gvalues.
--container_child_set
--container_child_get are var arg functions, but can just wrap many calls to single one


-- | Retrieves the 'ChildMeta' which contains the data about the container specific state for actor.
{# fun unsafe container_get_child_meta as ^
       `(ContainerClass container)' =>
       { withContainerClass* `container',
         withActor* `Actor' } ->
        `ChildMeta' newChildMeta* #}

