-- -*-haskell-*-
--  Clutter Container
--
--  Author : Matthew Arsenault
--
--  Created: 12 Sep 2009
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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

--TODO: Property list of children?

-- | ClutterContainer — An interface for implementing container actors
module Graphics.UI.Clutter.Container (
-- * Descriptionn
-- | 'Container' is an interface for writing actors containing other
-- 'Actor's. It provides a standard API for adding, removing and
-- iterating on every contained actor.
--
-- An actor implementing 'ContainerClass' is 'Group'.
--
-- 'Container' is available since Clutter 0.4
--

-- * Class Hierarchy
-- |
-- @
-- |  'GInterface'
-- |    +----'Container'
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
  containerChildSet,
  containerChildGet,

-- * Signals
  actorAdded,
  onActorAdded,
  afterActorAdded,

  actorRemoved,
  onActorRemoved,
  afterActorRemoved
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Animation #}
{# import Graphics.UI.Clutter.StoreValue #}
{# import Graphics.UI.Clutter.Signals #}

import C2HS
import Prelude
import qualified Prelude as P
import System.Glib.Attributes
import qualified System.Glib.GTypeConstants as GType

import Control.Monad (forM_)

-- | Adds an Actor to container. This function will emit the 'actorAdded' signal.
--   The actor should be parented to container. You cannot add a 'Actor' to more
--   than one 'Container'.
{# fun container_add_actor as ^
       `(ContainerClass container, ActorClass actor)' =>
           { withContainerClass* `container', withActorClass* `actor' } -> `()' #}

--TODO: add/removeList as var args function, to allow multiple types of the actor class in the list


-- | Removes actor from container. The
--   actor should be unparented When the actor has been removed, the
--  'actorRemoved' signal is emitted by container.
{# fun container_remove_actor as ^
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

-- | Calls callback for each child of container, including "internal"
--   children built in to the container itself that were never added by
--   the application.
containerForeachWithInternals :: (ContainerClass c) => c -> Callback -> IO ()
containerForeachWithInternals c func = withContainerClass c $ \cptr -> do
                                         funcPtr <- newCallback func
                                         {# call container_foreach #} cptr funcPtr nullPtr
                                         freeHaskellFunPtr funcPtr


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

-- | Sorts a container's children using their depth. This function
-- should not be normally used by applications.
{# fun unsafe container_sort_depth_order as ^
       `(ContainerClass container)' => { withContainerClass* `container' } -> `()' #}


--CHECKME: unsafe?
--CHECKME: What are "container specific properties" and does this make sense?
-- | Sets container specific properties on the child of a container.
--
-- [@container@] a Container
--
-- [@actor@] an Actor that is a child of container.
--
-- [@attributes@] List of attributes and their values to be set
--
-- * Since 0.8
--
containerChildSet :: (ContainerClass container, ActorClass child) => container
                     -> child
                     -> [AnimOp child]
                     -> IO ()
containerChildSet ctr chld ops = let func = {# call unsafe container_child_set_property #}
                                 in withContainerClass ctr $ \ctrPtr ->
                                      withActorClass chld $ \chldPtr ->
                                        forM_ ops $ \(attr :-> val) ->
                                                     withCString (P.show attr) $ \strPtr ->
                                                       withGenericValue val $ \valPtr ->
                                                         func ctrPtr chldPtr strPtr valPtr


--FIXME: ReadAttr? Also
containerChildGet :: (ContainerClass container, ActorClass child, GenericValueClass a) =>
                     container
                     -> child
                     -> Attr child a
                     -> IO a
containerChildGet ctr chld attr =  let func = {# call unsafe container_child_get_property #}
                                   in do (generic, _) <- withContainerClass ctr $ \ctrPtr ->
                                                           withActorClass chld $ \chldPtr ->
                                                             withCString (P.show attr) $ \strPtr ->
                                                               allocaTypedGValue GType.none $ \gvPtr ->
                                                                 func ctrPtr chldPtr strPtr gvPtr
                                         return (unsafeExtractGenericValue generic)


actorAdded :: (ContainerClass container) => Signal container (Actor -> IO ())
actorAdded = Signal (connect_OBJECT__NONE "actor-added")

onActorAdded, afterActorAdded :: ContainerClass a => a -> (Actor -> IO ()) -> IO (ConnectId a)
onActorAdded = connect_OBJECT__NONE "actor-added" False
afterActorAdded = connect_OBJECT__NONE "actor-added" True


actorRemoved :: (ContainerClass container) => Signal container (Actor -> IO ())
actorRemoved = Signal (connect_OBJECT__NONE "actor-removed")

onActorRemoved, afterActorRemoved :: ContainerClass a => a -> (Actor -> IO ()) -> IO (ConnectId a)
onActorRemoved = connect_OBJECT__NONE "actor-removed" False
afterActorRemoved = connect_OBJECT__NONE "actor-removed" True



