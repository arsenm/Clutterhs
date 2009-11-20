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

-- | BehaviourScale — A behaviour controlling scale
module Graphics.UI.Clutter.BehaviourScale (
-- * Description
-- | A 'BehaviourScale' interpolates actors size between two values.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Behaviour'
-- |           +----'BehaviourScale'
-- |
-- @

-- * Constructors
  behaviourScaleNew,

-- * Methods
  behaviourScaleSetBounds,
  behaviourScaleGetBounds

  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes


-- | Creates a new 'BehaviourScale' instance.
--
-- [@alpha@] an 'Alpha'
--
-- [@x_scale_start@] initial scale factor on the X axis
--
-- [@y_scale_start@] initial scale factor on the Y axis
--
-- [@x_scale_end@] final scale factor on the X axis
--
-- [@y_scale_end@] final scale factor on the Y axis
--
-- [@Returns@] the newly created 'BehaviourScale'
--
-- * Since 0.2
--
{# fun unsafe behaviour_scale_new as ^
       { withAlpha* `Alpha', `Double', `Double', `Double', `Double'} ->
       `BehaviourScale' newBehaviourScale* #}


-- | Sets the bounds used by scale behaviour.
--
-- [@scale@] a 'BehaviourScale'
--
-- [@x_scale_start@] initial scale factor on the X axis
--
-- [@y_scale_start@] initial scale factor on the Y axis
--
-- [@x_scale_end@] final scale factor on the X axis
--
-- [@y_scale_end@] final scale factor on the Y axis
--
-- * Since 0.6
--
{# fun unsafe behaviour_scale_set_bounds as ^
       { withBehaviourScale* `BehaviourScale', `Double', `Double', `Double', `Double'} -> `()' #}

-- | Retrieves the bounds used by scale behaviour.
--
-- [@scale@] a 'BehaviourScale'
--
-- [@Returns@] (initial scale factor on the X axis,
--              initial scale factor on the Y axis,
--              final scale factor on the X axis,
--              final scale factor on the Y axis)
--
-- * Since 0.4
--
{# fun unsafe behaviour_scale_get_bounds as ^
       { withBehaviourScale* `BehaviourScale',
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*} -> `()' #}

