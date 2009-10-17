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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.BehaviourOpacity (
                                           --behaviourOpacityNew,
                                           --behaviourOpacitySetBounds,
                                           --behaviourOpacityGetBounds,
                                           --behaviourOpacityBounds
                                            ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{-
{# fun unsafe behaviour_opacity_new as ^
       { withAlpha* `Alpha', `GUInt8', `GUInt8'} -> `BehaviourOpacity' newBehaviourOpacity* #}

{# fun unsafe behaviour_opacity_set_bounds as ^
       { withBehaviourOpacity* `BehaviourOpacity', `GUInt8', `GUInt8'} -> `()' #}

{# fun unsafe behaviour_opacity_get_bounds as ^
       { withBehaviourOpacity* `BehaviourOpacity',
         alloca- `GUInt8' peekSomethingConv*,
         alloca- `GUInt8' peekSomethingConv*} -> `()' #}
-}
--maybe make this work
--behaviourOpacityBounds :: Attr BehaviourOpacity (GUInt8, GUInt8)
--behaviourOpacityBounds = newAttr behaviourGetBounds behaviourSetBounds

