-- -*-haskell-*-
--  Clutter BehaviourScale
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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.BehaviourScale (
                                           behaviourScaleNew,
                                           behaviourScaleSetBounds,
                                           behaviourScaleGetBounds,
                                           behaviourScaleBounds
                                          ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe behaviour_scale_new as ^
       { withAlpha* `Alpha', `Double', `Double', `Double', `Double'} ->
       `BehaviourScale' newBehaviourScale* #}

{# fun unsafe behaviour_scale_set_bounds as ^
       { withBehaviourScale* `BehaviourScale', `Double', `Double', `Double', `Double'} -> `()' #}

{# fun unsafe behaviour_scale_get_bounds as ^
       { withBehaviourScale* `BehaviourScale',
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*} -> `()' #}

--CHECKME: Do I want this?
behaviourScaleBounds :: Attr BehaviourScale (Double, Double, Double, Double)
behaviourScaleBounds = newAttr behaviourScaleGetBounds (tup4ToF behaviourScaleSetBounds)



