-- -*-haskell-*-
--  Clutter Timeline
--
--  Author : Matthew Arsenault
--
--  Created: 21 Sep 2009
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

module Graphics.UI.Clutter.Timeline (
                                      timelineNew,

                                      timelineSetDuration,
                                      timelineGetDuration,
                                      timelineDuration
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties

--FIXME: GUInt
{# fun unsafe timeline_new as ^ { `Int' } -> `Timeline' newTimeline* #}

{# fun unsafe timeline_set_duration as ^ { withTimeline* `Timeline', `Int' } -> `()' #}
{# fun unsafe timeline_get_duration as ^ { withTimeline* `Timeline' } -> `Int' #}
timelineDuration :: Attr Timeline Int
timelineDuration = newAttr timelineGetDuration timelineSetDuration


