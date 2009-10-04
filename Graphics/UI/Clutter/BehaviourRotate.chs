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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.BehaviourRotate (
                                            behaviourRotateNew,

                                            behaviourRotateSetAxis,
                                            behaviourRotateGetAxis,
                                            behaviourRotateAxis,

                                            behaviourRotateSetDirection,
                                            behaviourRotateGetDirection,
                                            behaviourRotateDirection,

                                            behaviourRotateSetBounds,
                                            behaviourRotateGetBounds,
                                          --behaviourRotateBounds,

                                            behaviourRotateSetCenter,
                                            behaviourRotateGetCenter
                                          --behaviourRotateCenter
                                           ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

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

--TODO: maybe make this work
--behaviourRotateBounds :: Attr BehaviourRotate (Double, Double)
--behaviourRotateBounds = newAttr behaviourGetBounds behaviourSetBounds

{# fun unsafe behaviour_rotate_set_center as ^
       { withBehaviourRotate* `BehaviourRotate', `Int', `Int', `Int'} -> `()' #}

{# fun unsafe behaviour_rotate_get_center as ^
       { withBehaviourRotate* `BehaviourRotate',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}

--TODO: maybe make this work
--behaviourRotateCenter :: Attr BehaviourRotate (Int, Int)
--behaviourRotateCenter = newAttr behaviourGetCenter behaviourSetCenter

