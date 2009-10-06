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

module Graphics.UI.Clutter.Container (
                                      containerAddActor,
                                    --containerAddList
                                      containerRemoveActor,
                                    --containerRemove,
                                    --containerRemoveList,
                                      containerGetChildren,
                                    --containerForeach,
                                    --containerForeachWithInternals,
                                      containerFindChildByName,
                                      containerRaiseChild,
                                      containerLowerChild,
                                      containerSortDepthOrder
                                    --containerClassFindChildProperty,
                                    --containerClassListChildProperties,
                                    --containerChildSetProperty,
                                    --containerChildGetProperty,
                                    --containerChildSet,
                                    --containerChildGet,
                                    --containerGetChildMeta
                                   ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject


{# fun unsafe container_add_actor as ^
       `(ContainerClass container, ActorClass actor)' =>
           { withContainerClass* `container', withActorClass* `actor' } -> `()' #}


{# fun unsafe container_remove_actor as ^
       `(ContainerClass container, ActorClass actor)' =>
           { withContainerClass* `container', withActorClass* `actor' } -> `()' #}

{# fun unsafe container_get_children as ^
       `(ContainerClass container)' => { withContainerClass* `container' } -> `[Actor]' newActorList* #}


{# fun unsafe container_find_child_by_name as ^
       `(ContainerClass container)' => { withContainerClass* `container', `String' } -> `Actor' newActor* #}


{# fun unsafe container_raise_child as ^
       `(ContainerClass container, ActorClass a, ActorClass b)' =>
           { withContainerClass* `container', withActorClass* `a', withActorClass* `b' } -> `()' #}
{# fun unsafe container_lower_child as ^
       `(ContainerClass container, ActorClass a, ActorClass b)' =>
           { withContainerClass* `container', withActorClass* `a', withActorClass* `b' } -> `()' #}

{# fun unsafe container_sort_depth_order as ^
       `(ContainerClass container)' => { withContainerClass* `container' } -> `()' #}

