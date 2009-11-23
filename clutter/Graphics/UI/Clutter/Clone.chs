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

-- | ClutterClone — An actor that displays a clone of a source actor
module Graphics.UI.Clutter.Clone (
-- * Constructors
  cloneNew,
-- * Methods
  cloneSetSource,
  cloneGetSource,
-- * Attributes
  cloneSource
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import qualified Graphics.UI.Clutter.GTypes #} as CGT

import C2HS
import System.Glib.Attributes
import System.Glib.Properties

--TODO: CloneClass

-- | Creates a new 'Actor' which clones 'source'
{# fun unsafe clone_new as ^ `(ActorClass a)' => { withActorClass* `a' } -> `Clone' newClone* #}

-- | Retrieves the source 'Actor' being cloned by clone
{# fun unsafe clone_get_source as ^ { withClone* `Clone' } -> `Actor' newActor* #}

-- | Sets source as the source actor to be cloned by clone.
{# fun unsafe clone_set_source as ^ `(ActorClass a)' => { withClone* `Clone', withActorClass* `a' } -> `()' #}

--CHECKME: Actor vs. ActorClass. Could get anything back.
cloneSource :: (ActorClass self) => Attr Clone self
cloneSource = newAttrFromObjectProperty "source" CGT.actor


