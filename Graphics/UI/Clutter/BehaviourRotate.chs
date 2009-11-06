-- -*-haskell-*-
--  Clutter BehaviourRotate
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


-- | BehaviourRotate — A behaviour controlling rotation
module Graphics.UI.Clutter.BehaviourRotate (
-- * Description
-- | A 'BehaviourRotate' rotate actors between a starting and ending
--   angle on a given axis.
--
-- 'BehaviourRotate' is available since version 0.4.
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Behaviour'
-- |         +----'BehaviourRotate'
-- |
-- @

-- * Constructors
  behaviourRotateNew,

-- * Methods
  behaviourRotateSetAxis,
  behaviourRotateGetAxis,

  behaviourRotateSetDirection,
  behaviourRotateGetDirection,

  behaviourRotateSetBounds,
  behaviourRotateGetBounds,

  behaviourRotateSetCenter,
  behaviourRotateGetCenter,

-- * Attributes
  behaviourRotateAxis,
  behaviourRotateDirection,
  behaviourRotateBounds,
  behaviourRotateCenter
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes


-- | Creates a new 'BehaviourRotate'. This behaviour will rotate
--   actors bound to it on axis, following direction, between
--   angle_start and angle_end. Angles >= 360 degrees will be clamped
--   to the canonical interval <0, 360), if angle_start == angle_end,
--   the behaviour will carry out a single rotation of 360 degrees.
--
-- [@alpha@] @Just@ an 'Alpha', or @Nothing@
--
-- [@axis@] the rotation axis
--
-- [@direction@] the rotation direction
--
-- [@angle_start@] the starting angle in degrees, between 0 and 360.
--
-- [@angle_end@] the final angle in degrees, between 0 and 360.
--
-- [@Returns@] the newly created 'BehaviourRotate'.
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_new as ^
       { withAlpha* `Alpha',
         cFromEnum `RotateAxis',
         cFromEnum `RotateDirection',
         `Double',
         `Double' } ->
       `BehaviourRotate' newBehaviourRotate* #}

{# fun unsafe behaviour_rotate_set_axis as ^
       { withBehaviourRotate* `BehaviourRotate', cFromEnum `RotateAxis'} -> `()' #}
{# fun unsafe behaviour_rotate_get_axis as ^
       { withBehaviourRotate* `BehaviourRotate' } -> `RotateAxis' cToEnum #}
behaviourRotateAxis :: Attr BehaviourRotate RotateAxis
behaviourRotateAxis = newAttr behaviourRotateGetAxis behaviourRotateSetAxis

{# fun unsafe behaviour_rotate_set_direction as ^
       { withBehaviourRotate* `BehaviourRotate', cFromEnum `RotateDirection'} -> `()' #}
{# fun unsafe behaviour_rotate_get_direction as ^
       { withBehaviourRotate* `BehaviourRotate' } -> `RotateDirection' cToEnum #}
behaviourRotateDirection :: Attr BehaviourRotate RotateDirection
behaviourRotateDirection = newAttr behaviourRotateGetDirection behaviourRotateSetDirection

{# fun unsafe behaviour_rotate_set_bounds as ^
       { withBehaviourRotate* `BehaviourRotate', `Double', `Double'} -> `()' #}

{# fun unsafe behaviour_rotate_get_bounds as ^
       { withBehaviourRotate* `BehaviourRotate',
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv* } -> `()' #}

behaviourRotateBounds :: Attr BehaviourRotate (Double, Double)
behaviourRotateBounds = newAttr behaviourRotateGetBounds (tup2ToF behaviourRotateSetBounds)

{# fun unsafe behaviour_rotate_set_center as ^
       { withBehaviourRotate* `BehaviourRotate', `Int', `Int', `Int'} -> `()' #}

{# fun unsafe behaviour_rotate_get_center as ^
       { withBehaviourRotate* `BehaviourRotate',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}

behaviourRotateCenter :: Attr BehaviourRotate (Int, Int, Int)
behaviourRotateCenter = newAttr behaviourRotateGetCenter (tup3ToF behaviourRotateSetCenter)

