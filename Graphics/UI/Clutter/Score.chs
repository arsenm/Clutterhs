-- -*-haskell-*-
--  Clutter Score
--
--  Author : Matthew Arsenault
--
--  Created: 9 Oct 2009
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Score (
-- |
-- @
-- |  'GObject'
-- |   +----'Score'
-- @

-- * Constructors
  scoreNew,

-- * Methods
  scoreSetLoop,
  scoreGetLoop,

  scoreAppend,
  scoreAppendAtMarker,

  scoreRemove,
  scoreRemoveAll,

  scoreGetTimeline,
  scoreListTimelines,

  scoreStart,
  scorePause,
  scoreStop,
  scoreIsPlaying,
  scoreRewind,

-- * Attributes
  scoreLoop,

-- * Signals
{-
--FIXME: Export name conflict
  onCompleted,
  afterCompleted,
  completed,
  onPaused,
  afterPaused,
  paused,
  onStarted,
  afterStarted,
  started,

  onTimelineCompleted,
  afterTimelineCompleted,
  timelineCompleted,
  onTimelineStarted,
  afterTimelineStarted,
  timelineStarted
 -}
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe score_new as ^ { } -> `Score' newScore* #}

{# fun unsafe score_set_loop as ^ { withScore* `Score', `Bool' } -> `()' #}
{# fun unsafe score_get_loop as ^ { withScore* `Score' } -> `Bool' #}
scoreLoop :: Attr Score Bool
scoreLoop = newAttr scoreGetLoop scoreSetLoop

--TODO: GULong. Also type alias for the id whatsit
{# fun unsafe score_append as ^
       { withScore* `Score', withTimeline* `Timeline', withTimeline* `Timeline' } -> `Word64' #}

{# fun unsafe score_append_at_marker as ^
       { withScore* `Score', withTimeline* `Timeline', `String', withTimeline* `Timeline' } -> `Word64' #}

{# fun unsafe score_remove as ^ { withScore* `Score', `Word64' } -> `()' #}
{# fun unsafe score_remove_all as ^ { withScore* `Score' } -> `()' #}

{# fun unsafe score_get_timeline as ^
       { withScore* `Score', `Word64' } -> `Timeline' newTimeline* #}

{# fun unsafe score_list_timelines as ^
       { withScore* `Score'} -> `[Timeline]' newTimelineList* #}

{# fun unsafe score_start as ^ { withScore* `Score' } -> `()' #}
{# fun unsafe score_pause as ^ { withScore* `Score' } -> `()' #}
{# fun unsafe score_stop as ^ { withScore* `Score' } -> `()' #}

--TODO: ReadOnly Attr?
{# fun unsafe score_is_playing as ^ { withScore* `Score' } -> `Bool' #}

{# fun unsafe score_rewind as ^ { withScore* `Score' } -> `()' #}


onCompleted, afterCompleted :: Score -> IO () -> IO (ConnectId Score)
onCompleted = connect_NONE__NONE "completed" False
afterCompleted = connect_NONE__NONE "completed" True

completed :: Signal Score (IO ())
completed = Signal (connect_NONE__NONE "completed")


onPaused, afterPaused :: Score -> IO () -> IO (ConnectId Score)
onPaused = connect_NONE__NONE "paused" False
afterPaused = connect_NONE__NONE "paused" True

paused :: Signal Score (IO ())
paused = Signal (connect_NONE__NONE "paused")


onStarted, afterStarted :: Score -> IO () -> IO (ConnectId Score)
onStarted = connect_NONE__NONE "started" False
afterStarted = connect_NONE__NONE "started" True

started :: Signal Score (IO ())
started = Signal (connect_NONE__NONE "started")


onTimelineCompleted, afterTimelineCompleted :: Score -> (Timeline -> IO ()) -> IO (ConnectId Score)
onTimelineCompleted = connect_OBJECT__NONE "timeline_completed" False
afterTimelineCompleted = connect_OBJECT__NONE "timeline_completed" True

timelineCompleted :: Signal Score (Timeline -> IO ())
timelineCompleted = Signal (connect_OBJECT__NONE "timeline_completed")


onTimelineStarted, afterTimelineStarted :: Score -> (Timeline -> IO ()) -> IO (ConnectId Score)
onTimelineStarted = connect_OBJECT__NONE "timeline_started" False
afterTimelineStarted = connect_OBJECT__NONE "timeline_started" True

timelineStarted :: Signal Score (Timeline -> IO ())
timelineStarted = Signal (connect_OBJECT__NONE "timeline_started")


