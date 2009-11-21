-- -*-haskell-*-
--  Clutter ChildMeta
--
--  Author : Matthew Arsenault
--
--  Created: 2 Oct 2009
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.ChildMeta (
                                      childMetaGetContainer
                                    --childMetaContainer,
                                    --childMetaGetActor,
                                    --childMetaActor
                                     ) where

{# import Graphics.UI.Clutter.Types #}

--import System.Glib.Attributes

childMetaGetContainer = undefined

{-
--TODO: not use this, since it's just a simple struct

{# fun unsafe child_meta_get_container as ^ { withChildMeta* `ChildMeta' } -> `Container' newContainer* #}
childMetaContainer :: (ChildMetaClass cm) => ReadAttr cm Container
childMetaContainer = readAttr childMetaGetContainer


{# fun unsafe child_meta_get_actor as ^ { withChildMeta* `ChildMeta' } -> `Actor' newActor* #}
childMetaActor :: (ChildMetaClass cm) => ReadAttr cm Actor
childMetaActor = readAttr childMetaGetActor

-}

