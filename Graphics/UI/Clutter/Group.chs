-- -*-haskell-*-
--  Clutter Group
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Group (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GInitiallyUnowned'
-- |         +----'Actor'
-- |               +----'Group'
-- |                     +----'Stage'
-- @

-- * Constructors
  groupNew,

-- * Methods
  groupRemoveAll,
  groupGetNChildren,
  groupGetNthChild
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS

{# fun unsafe group_new as ^ {} -> `Group' newGroup* #}
{# fun unsafe group_remove_all as ^ { withGroup* `Group' } -> `()' #}
{# fun unsafe group_get_n_children as ^ { withGroup* `Group' } -> `Int' #}
{# fun unsafe group_get_nth_child as ^ { withGroup* `Group', `Int' } -> `Actor' newActor* #}


