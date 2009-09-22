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
                                      timelineClone,

                                      timelineSetDuration,
                                      timelineGetDuration,
                                      timelineDuration,

                                      timelineSetLoop,
                                      timelineGetLoop,
                                      timelineLoop,

                                      timelineSetDelay,
                                      timelineGetDelay,
                                      timelineDelay,

                                      timelineSetDirection,
                                      timelineGetDirection,
                                      timelineDirection,

                                      timelineStart,
                                      timelinePause,
                                      timelineStop,
                                      timelineRewind,
                                      timelineSkip,
                                      timelineAdvance,
                                      timelineGetElapsedTime,
                                      timelineGetDelta,
                                      timelineGetProgress,
                                      timelineIsPlaying,

                                      timelineAddMarkerAtTime,
                                      timelineHasMarker,
                                    --timelineListMarkers,
                                      timelineRemoveMarker,
                                      timelineAdvanceToMarker
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties

--FIXME: GUInt
{# fun unsafe timeline_new as ^ { `Int' } -> `Timeline' newTimeline* #}
{# fun unsafe timeline_clone as ^ { withTimeline* `Timeline'} -> `Timeline' newTimeline* #}

{# fun unsafe timeline_set_duration as ^ { withTimeline* `Timeline', `Int' } -> `()' #}
{# fun unsafe timeline_get_duration as ^ { withTimeline* `Timeline' } -> `Int' #}
timelineDuration :: Attr Timeline Int
timelineDuration = newAttr timelineGetDuration timelineSetDuration

{# fun unsafe timeline_set_loop as ^ { withTimeline* `Timeline', `Bool' } -> `()' #}
{# fun unsafe timeline_get_loop as ^ { withTimeline* `Timeline' } -> `Bool' #}
timelineLoop :: Attr Timeline Bool
timelineLoop = newAttr timelineGetLoop timelineSetLoop

{# fun unsafe timeline_set_delay as ^ { withTimeline* `Timeline', `Int' } -> `()' #}
{# fun unsafe timeline_get_delay as ^ { withTimeline* `Timeline' } -> `Int' #}
timelineDelay :: Attr Timeline Int
timelineDelay = newAttr timelineGetDelay timelineSetDelay

{# fun unsafe timeline_set_direction as ^
       { withTimeline* `Timeline', cFromEnum `TimelineDirection' } -> `()' #}
{# fun unsafe timeline_get_direction as ^
       { withTimeline* `Timeline' } -> `TimelineDirection' cToEnum #}
timelineDirection :: Attr Timeline TimelineDirection
timelineDirection = newAttr timelineGetDirection timelineSetDirection

{# fun unsafe timeline_start as ^ { withTimeline* `Timeline' } -> `()' #}
{# fun unsafe timeline_pause as ^ { withTimeline* `Timeline' } -> `()' #}
{# fun unsafe timeline_stop as ^ { withTimeline* `Timeline' } -> `()' #}
{# fun unsafe timeline_rewind as ^ { withTimeline* `Timeline' } -> `()' #}

--TODO: GUInt
{# fun unsafe timeline_skip as ^ { withTimeline* `Timeline', `Int' } -> `()' #}
{# fun unsafe timeline_advance as ^ { withTimeline* `Timeline', `Int' } -> `()' #}

{# fun unsafe timeline_get_elapsed_time as ^ { withTimeline* `Timeline' } -> `Int' #}
{# fun unsafe timeline_get_delta as ^ { withTimeline* `Timeline' } -> `Int' #}

{# fun unsafe timeline_get_progress as ^ { withTimeline* `Timeline' } -> `Double' #}

{# fun unsafe timeline_is_playing as ^ { withTimeline* `Timeline' } -> `Bool' #}

{# fun unsafe timeline_add_marker_at_time as ^
       { withTimeline* `Timeline', `String', `Int' } -> `()' #}

{# fun unsafe timeline_has_marker as ^ { withTimeline* `Timeline', `String' } -> `Bool' #}

--TODO: Get list from array of size n
--{# fun unsafe timeline_list_markers as ^
--  { withTimeline* `Timeline', `Int', `String' } -> `[String]' #}

{# fun unsafe timeline_remove_marker as ^ { withTimeline* `Timeline', `String' } -> `()' #}
{# fun unsafe timeline_advance_to_marker as ^ { withTimeline* `Timeline', `String' } -> `()' #}

