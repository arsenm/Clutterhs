-- -*-haskell-*-
--  Clutter BehaviourOpacity
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

module Graphics.UI.Clutter.BehaviourOpacity (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Behaviour'
-- |         +----'BehaviourOpacity'
-- |
-- @

-- * Constructors
  behaviourOpacityNew,

-- * Methods
  behaviourOpacitySetBounds,
  behaviourOpacityGetBounds
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS

-- | Creates a new 'BehaviourOpacity' object, driven by alpha which
--   controls the opacity property of every actor, making it change in
--   the interval between opacity_start and opacity_end.
--
-- [@alpha@] @Just@ an 'Alpha' or @Nothing@
--
-- [@opacity_start@] minimum level of opacity
--
-- [@opacity_end@] maximum level of opacity
--
-- [@Returns@] the newly created 'BehaviourOpacity'
--
-- * Since 0.2
--
{# fun unsafe behaviour_opacity_new as ^
       { withMaybeAlpha* `Maybe Alpha', `Word8', `Word8'} -> `BehaviourOpacity' newBehaviourOpacity* #}


-- | Sets the initial and final levels of the opacity applied by
--   behaviour on each actor it controls.
--
-- [@behaviour@] a 'BehaviourOpacity'
--
-- [@opacity_start@] minimum level of opacity
--
-- [@opacity_end@] maximum level of opacity
--
-- * Since 0.6
--
{# fun unsafe behaviour_opacity_set_bounds as ^
       { withBehaviourOpacity* `BehaviourOpacity', `Word8', `Word8'} -> `()' #}

-- | Gets the initial and final levels of the opacity applied by
--   behaviour on each actor it controls.
--
-- [@behaviour@] a 'BehaviourOpacity'
--
-- [@Returns@] (minimum level of opacity, maximum level of opacity)
--
-- * Since 0.6
--
{# fun unsafe behaviour_opacity_get_bounds as ^
       { withBehaviourOpacity* `BehaviourOpacity',
         alloca- `Word8' peekIntConv*,
         alloca- `Word8' peekIntConv*} -> `()' #}

