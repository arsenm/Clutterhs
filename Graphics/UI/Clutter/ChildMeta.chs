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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.ChildMeta (
                                    --childMetaGetContainer,
                                    --childMetaContainer,
                                    --childMetaGetActor,
                                    --childMetaActor
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{-
{# fun unsafe child_meta_get_container as ^ { withChildMeta* `ChildMeta' } -> newContainer* `Container' #}
childMetaContainer :: (ActorClass self) => ReadAttr self Container
childMetaContainer = readAttr childMetaContainer

{# fun unsafe child_meta_get_actor as ^ { withChildMeta* `ChildMeta' } -> newContainer* `Container' #}
childMetaActor :: (ActorClass self) => ReadAttr self Actor
childMetaActor = readAttr childMetaContainer
-}


