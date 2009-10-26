-- -*-haskell-*-
--  Clutter BehaviourDepth
--
--  Author : Matthew Arsenault
--
--  Created: 3 Oct 2009
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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.BehaviourDepth (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Behaviour'
-- |         +----'BehaviourDepth'
-- |
-- @

-- * Constructors
  behaviourDepthNew,

-- * Methods
  behaviourDepthSetBounds,
  behaviourDepthGetBounds,

-- * Attributes
  behaviourDepthBounds
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes

{# fun unsafe behaviour_depth_new as ^
       { withAlpha* `Alpha', `Int', `Int' } -> `BehaviourDepth' newBehaviourDepth* #}

{# fun unsafe behaviour_depth_set_bounds as ^
       { withBehaviourDepth* `BehaviourDepth', `Int', `Int' } -> `()' #}

{# fun unsafe behaviour_depth_get_bounds as ^
       { withBehaviourDepth* `BehaviourDepth',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv*
       } -> `()' #}

behaviourDepthBounds :: Attr BehaviourDepth (Int, Int)
behaviourDepthBounds = newAttr behaviourDepthGetBounds (tup2ToF behaviourDepthSetBounds)

