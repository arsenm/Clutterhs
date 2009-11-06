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

-- | Group — Actor class containing multiple children. actors.
module Graphics.UI.Clutter.Group (
-- * Description

-- | A 'Group' is an Actor which contains multiple child actors
--   positioned relative to the ClutterGroup position. Other
--   operations such as scaling, rotating and clipping of the group
--   will apply to the child actors.
--
--  A 'Group'\'s size is defined by the size and position of its
--  children. Resize requests via the Actor API will be ignored.
--

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


-- | Create a new 'Group'.
--
-- [@Returns@] the newly created 'Group' actor
--
{# fun unsafe group_new as ^ {} -> `Group' newGroup* #}


-- | Removes all children actors from the 'Group'.
--
-- [@group@] A 'Group'
--
{# fun unsafe group_remove_all as ^ { withGroup* `Group' } -> `()' #}

-- | Gets the number of actors held in the group.
--
-- [@self@] A 'Group'
--
-- [@Returns@] The number of child actors held in the group.
--
-- * Since 0.2
--
{# fun unsafe group_get_n_children as ^ { withGroup* `Group' } -> `Int' #}

-- | Gets a groups child held at index in stack.
--
-- [@self@] A 'Group'
--
-- [@index@] the position of the requested actor.
--
-- [@Returns@] transfer none. transfer none.
--
-- * Since 0.2
--
{# fun unsafe group_get_nth_child as ^ { withGroup* `Group', `Int' } -> `Actor' newActor* #}

