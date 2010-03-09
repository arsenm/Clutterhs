-- -*-haskell-*-
--  Clutter Clone
--
--  Author : Matthew Arsenault
--
--  Created: 2 Oct 2009
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- | Clone — An actor that displays a clone of a source actor
module Graphics.UI.Clutter.Clone (
-- * Description
--
-- | 'Clone' is an Actor which draws with the paint function of
-- another actor, scaled to fit its own allocation.
--
-- 'Clone' can be used to efficiently clone any other actor.
--
-- * Note
--
-- This is different from 'textureNewFromActor' which requires support
-- for FBOs in the underlying GL implementation.
--
-- 'Clone' is available since Clutter 1.0
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'Clone'
-- @
--

-- * Types
  Clone,

-- * Constructors
  cloneNew,
-- * Methods
  cloneSetSource,
  cloneGetSource,
-- * Attributes
  cloneSource
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes


-- | Creates a new 'Actor' which clones 'source'
--
-- [@source@] @Just@ an Actor, or @Nothing@
--
-- [@Returns@] the newly created 'Clone'
--
-- * Since 1.0
--
cloneNew :: (ActorClass a) => Maybe a -> IO (Clone a)
cloneNew actor = withMaybeActorClass actor $ \actorPtr ->
                   liftM (mkClone (undefined::a))
                         (newCloneRaw =<< {# call unsafe clone_new #} actorPtr)


-- | Sets source as the source actor to be cloned by clone.
--
-- [@clone@] a 'Clone'
--
-- [@source@] @Just@ an Actor, or @Nothing@
--
-- * Since 1.0
--
{# fun unsafe clone_set_source as ^ `(ActorClass a)' =>
    { withClone* `Clone a', withMaybeActorClass* `Maybe a' } -> `()' #}

-- | Retrieves the source 'Actor' being cloned by clone
--
-- [@clone@] a 'Clone'
--
-- [@Returns@] the actor source for the clone.
--
-- * Since 1.0
--
cloneGetSource :: (ActorClass a) => Clone a -> IO (Maybe a)
cloneGetSource clone = withClone clone $ \clonePtr ->
                         liftM (liftM unsafeCastActor)
                               (maybeNewActor =<< {# call unsafe clone_get_source #} clonePtr)

-- | This property specifies the source actor being cloned.
--
-- * Since 1.0
--
cloneSource :: (ActorClass a) => Attr (Clone a) (Maybe a)
cloneSource = newNamedAttr "source" cloneGetSource cloneSetSource


