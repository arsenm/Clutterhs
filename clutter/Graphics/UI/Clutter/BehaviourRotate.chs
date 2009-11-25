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

-- * Types
  BehaviourRotate,
  BehaviourRotateClass,
  RotateAxis(..),
  RotateDirection(..),

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
  behaviourRotateAngleEnd,
  behaviourRotateAngleStart,
  behaviourRotateAxis,
  behaviourRotateCenterX,
  behaviourRotateCenterY,
  behaviourRotateCenterZ,
  behaviourRotateDirection
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes
import System.Glib.Properties


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
       { withMaybeAlpha* `Maybe Alpha',
         cFromEnum `RotateAxis',
         cFromEnum `RotateDirection',
         `Double',
         `Double' } ->
       `BehaviourRotate' newBehaviourRotate* #}


-- | Sets the axis used by the rotate behaviour.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@axis@] a 'RotateAxis'
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_set_axis as ^
       { withBehaviourRotate* `BehaviourRotate', cFromEnum `RotateAxis'} -> `()' #}

-- | Retrieves the 'RotateAxis' used by the rotate behaviour.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@Returns@] the rotation axis
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_get_axis as ^
       { withBehaviourRotate* `BehaviourRotate' } -> `RotateAxis' cToEnum #}


-- | Sets the rotation direction used by the rotate behaviour.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@direction@] the rotation direction
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_set_direction as ^
       { withBehaviourRotate* `BehaviourRotate', cFromEnum `RotateDirection'} -> `()' #}

-- | Retrieves the 'RotateDirection' used by the rotate behaviour.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@Returns@] the rotation direction
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_get_direction as ^
       { withBehaviourRotate* `BehaviourRotate' } -> `RotateDirection' cToEnum #}

-- | Sets the initial and final angles of a rotation behaviour; angles
--   \>= 360 degrees get clamped to the canonical interval <0, 360).
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@angle_start@] initial angle in degrees, between 0 and 360.
--
-- [@angle_end@] final angle in degrees, between 0 and 360.
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_set_bounds as ^
       { withBehaviourRotate* `BehaviourRotate', `Double', `Double'} -> `()' #}


-- | Retrieves the rotation boundaries of the rotate behaviour.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@Returns@] (angle_start, angle_end)
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_get_bounds as ^
       { withBehaviourRotate* `BehaviourRotate',
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv* } -> `()' #}


-- | Sets the center of rotation. The coordinates are relative to the
--   plane normal to the rotation axis set with
--   'behaviourRotateSetAxis'.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@x@] X axis center of rotation
--
-- [@y@] Y axis center of rotation
--
-- [@z@] Z axis center of rotation
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_set_center as ^
       { withBehaviourRotate* `BehaviourRotate', `Int', `Int', `Int'} -> `()' #}

-- | Retrieves the center of rotation set using
--   'behaviourRotateSetCenter'.
--
-- [@rotate@] a 'BehaviourRotate'
--
-- [@Returns@] (X center of rotation, Y center of rotation, Z center of rotation)
--
-- * Since 0.4
--
{# fun unsafe behaviour_rotate_get_center as ^
       { withBehaviourRotate* `BehaviourRotate',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}



-- | The final angle to where the rotation should end.
--
-- Allowed values: [0,360]
--
-- Default value: 0
--
-- * Since 0.4
--
behaviourRotateAngleEnd :: Attr BehaviourRotate Double
behaviourRotateAngleEnd = newAttrFromDoubleProperty "angle-end"


-- | The initial angle from whence the rotation should start.
--
-- Allowed values: [0,360]
--
-- Default value: 0
--
-- * Since 0.4
--
behaviourRotateAngleStart :: Attr BehaviourRotate Double
behaviourRotateAngleStart = newAttrFromDoubleProperty "angle-start"


-- | The axis of rotation.
--
-- Default value: 'ZAxis'
--
-- * Since 0.4
--
behaviourRotateAxis :: Attr BehaviourRotate RotateAxis
behaviourRotateAxis = newNamedAttr "axis" behaviourRotateGetAxis behaviourRotateSetAxis

-- | The x center of rotation.
--
-- Allowed values: >= -2147483647
--
-- Default value: 0
--
-- * Since 0.4
--
behaviourRotateCenterX :: Attr BehaviourRotate Int
behaviourRotateCenterX = newAttrFromIntProperty "center-x"


-- | The y center of rotation.
--
-- Allowed values: >= -2147483647
--
-- Default value: 0
--
-- * Since 0.4
--
behaviourRotateCenterY :: Attr BehaviourRotate Int
behaviourRotateCenterY = newAttrFromIntProperty "center-y"


-- | The z center of rotation.
--
-- Allowed values: >= -2147483647
--
-- Default value: 0
--
-- * Since 0.4
--
behaviourRotateCenterZ :: Attr BehaviourRotate Int
behaviourRotateCenterZ = newAttrFromIntProperty "center-z"

-- | The direction of the rotation.
--
-- Default value: 'RotateCw'
--
-- * Since 0.4
--
behaviourRotateDirection :: Attr BehaviourRotate RotateDirection
behaviourRotateDirection = newNamedAttr "direction" behaviourRotateGetDirection behaviourRotateSetDirection

