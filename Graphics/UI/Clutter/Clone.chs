-- -*-haskell-*-
--  Clutter Clone
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

module Graphics.UI.Clutter.Clone (
                                  cloneNew
                                --cloneSetSource,
                                --cloneGetSource,
                                --cloneSource
                                 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe clone_new as ^ `(ActorClass a)' => { withActorClass* `a' } -> `Clone' newClone* #}


{-
--Attribute is unhappy, class Actor vs. ActorClass.
{# fun unsafe clone_get_source as ^ { withClone* `Clone' } -> `Actor' newActor* #}
{# fun unsafe clone_set_source as ^ `(ActorClass a)' => { withClone* `Clone', withActorClass* `a' } -> `()' #}
cloneSource :: (ActorClass self) => Attr Clone self
cloneSource = newAttr cloneGetSource cloneSetSource
-}

