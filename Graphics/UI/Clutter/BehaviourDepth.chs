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

-- | BehaviourDepth — A behaviour controlling the Z position
module Graphics.UI.Clutter.BehaviourDepth (
-- * Description
-- | 'BehaviourDepth' is a simple 'Behaviour' controlling the depth of a
--   set of actors between a start and end depth.
--
-- 'BehaviourDepth' is available since Clutter 0.4.

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

-- | Creates a new 'BehaviourDepth' which can be used to control
--   the 'Actor':depth property of a set of Actors.
--
-- [@alpha@]  @Just@ an Alpha or @Nothing@
--
-- [@depth_start@] initial value of the depth
--
-- [@depth_end@] final value of the depth
--
-- [@Returns@] the newly created behaviour
--
-- * Since 0.4
--
{# fun unsafe behaviour_depth_new as ^
       { withMaybeAlpha* `Maybe Alpha', `Int', `Int' } -> `BehaviourDepth' newBehaviourDepth* #}


-- | Sets the boundaries of the behaviour.
--
-- [@behaviour@] a 'BehaviourDepth'
--
-- [@depth_start@] initial value of the depth
--
-- [@depth_end@] final value of the depth
--
-- * Since 0.6
--
{# fun unsafe behaviour_depth_set_bounds as ^
       { withBehaviourDepth* `BehaviourDepth', `Int', `Int' } -> `()' #}


-- | Gets the boundaries of the behaviour
--
-- [@behaviour@] a 'BehaviourDepth'
--
-- [@Returns@] (depth_start, depth_end)
--
-- * Since 0.6
--
{# fun unsafe behaviour_depth_get_bounds as ^
       { withBehaviourDepth* `BehaviourDepth',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv*
       } -> `()' #}

-- | Start and end depth level to apply to the actors
--
-- * Since 0.4
--
behaviourDepthBounds :: Attr BehaviourDepth (Int, Int)
behaviourDepthBounds = newAttr behaviourDepthGetBounds (tup2ToF behaviourDepthSetBounds)

