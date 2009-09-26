-- -*-haskell-*-
--  Clutter Alpha
--
--  Author : Matthew Arsenault
--
--  Created: 26 Sep 2009
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

module Graphics.UI.Clutter.Alpha (
                                  alphaNew,
                                  alphaNewFull,

                                  alphaSetMode,
                                  alphaGetMode,
                                  alphaMode
                                 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes


{# fun unsafe alpha_new as ^ {} -> `Alpha' newAlpha* #}
{# fun unsafe alpha_new_full as ^
       { withTimeline* `Timeline', cFromEnum `AnimationMode' } -> `Alpha' newAlpha* #}

{# fun unsafe alpha_set_mode as ^ { withAlpha* `Alpha', cFromEnum `AnimationMode' } -> `()' #}
{# fun unsafe alpha_get_mode as ^ { withAlpha* `Alpha' } -> `AnimationMode' cToEnum #}
alphaMode :: Attr Alpha AnimationMode
alphaMode = newAttr alphaGetMode alphaSetMode
