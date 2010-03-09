-- -*-haskell-*-
--  Clutter Score
--
--  Author : Matthew Arsenault
--
--  Created: 9 Oct 2009
--
--  Copyright (C) 2009-2010 Matthew Arsenault
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

-- | Score - Controller for multiple timelines
module Graphics.UI.Clutter.Score (
-- * Description
-- | 'Score' is a base class for sequencing multiple timelines in
--  order. Using 'Score' it is possible to start multiple timelines at
--  the same time or launch multiple timelines when a particular
--  timeline has emitted the 'Timeline'::completed signal.
--
-- Each time a 'Timeline' is started and completed, a signal will be
-- emitted.
--
-- For example, this code will start two 'Timeline's after a third
-- timeline terminates:
--
-- @
--  do
--    timeline1 \<\- timelineNew 1000
--    timeline2 \<\- timelineNew 500
--    timeline3 \<\- timelineNew 500
--    score \<\- scoreNew
--    scoreAppend score Nothing timeline1
--    scoreAppend score \(Just timeline1\) timeline2
--    scoreAppend score \(Just timeline1\) timeline3
--    scoreStart score
-- @
--
-- New timelines can be appended to the 'Score' using 'scoreAppend'
-- and removed using 'scoreRemove'.
--
-- Timelines can also be appended to a specific marker on the parent
-- timeline, using 'scoreAppendAtMarker'.
--
-- The score can be cleared using 'scoreRemoveAll'.
--
-- The list of timelines can be retrieved using 'scoreListTimelines'.
--
-- The score state is controlled using 'scoreStart', 'scorePause',
-- 'scoreStop' and 'scoreRewind'. The state can be queried using
-- 'scoreIsPlaying'.
--
-- Score is available since Clutter 0.6
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Score'
-- @
--

-- * Types
  Score,
  ScoreClass,

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
  onTimelineCompleted,
  afterTimelineCompleted,
  timelineCompleted,
  onTimelineStarted,
  afterTimelineStarted,
  timelineStarted
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes


-- | Creates a new 'Score'. A 'Score' is an object that can hold
--   multiple 'Timelines' in a sequential order.
--
-- [@Returns@] the newly created 'Score'
--
-- * Since 0.6
--
{# fun unsafe score_new as ^ { } -> `Score' newScore* #}

-- | Sets whether score should loop. A looping 'Score' will start from
--   its initial state after the ::'completed' signal has been fired.
--
-- [@score@] a 'Score'
--
-- [@loop@] @True@ for enable looping
--
-- * Since 0.6
--
{# fun unsafe score_set_loop as ^ { withScore* `Score', `Bool' } -> `()' #}

-- | Gets whether score is looping
--
-- [@score@] a 'Score'
--
-- [@Returns@] @True@ if the score is looping
--
-- * Since 0.6
--
{# fun unsafe score_get_loop as ^ { withScore* `Score' } -> `Bool' #}

scoreLoop :: Attr Score Bool
scoreLoop = newNamedAttr "score" scoreGetLoop scoreSetLoop


-- | Appends a timeline to another one existing in the score; the
--   newly appended timeline will be started when parent is complete.
--
-- If parent is @Nothing@, the new 'Timeline' will be started when
-- 'scoreStart' is called.
--
-- [@score@] a Score
--
-- [@parent@] a @Just@ a 'Timeline' in the score, or @Nothing@
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] the id of the 'Timeline' inside the score, or 0 on
-- failure. The returned id can be used with 'scoreRemove' or
-- 'scoreGetTimeline'.
--
-- * Since 0.6
--
{# fun unsafe score_append as ^
       { withScore* `Score', withMaybeTimeline* `Maybe Timeline', withTimeline* `Timeline' } -> `Word' cIntConv #}

-- | Appends timeline at the given marker_name on the parent
--   'Timeline'.
--
-- If you want to append timeline at the end of parent, use 'scoreAppend'.
--
-- [@score@] a 'Score'
--
-- [@parent@] the parent 'Timeline'
--
-- [@marker_name@] the name of the marker to use
--
-- [@timeline@] the 'Timeline' to append
--
-- [@Returns@] the id of the 'Timeline' inside the score, or 0 on
-- failure. The returned id can be used with 'scoreRemove' or
-- 'scoreGetTimeline'.
--
-- * Since 0.8
--
{# fun unsafe score_append_at_marker as ^
       { withScore* `Score', withTimeline* `Timeline', `String', withTimeline* `Timeline' } -> `Word' cIntConv #}


-- | Removes the 'Timeline' with the given id inside score. If the
--   timeline has other timelines attached to it, those are removed as
--   well.
--
-- [@score@] a 'Score'
--
-- [@id@] the id of the timeline to remove
--
-- * Since 0.6
--
{# fun unsafe score_remove as ^ { withScore* `Score', cIntConv `Word' } -> `()' #}

-- | Removes all the timelines inside score.
--
-- [@score@] a 'Score'
--
-- * Since 0.6
--
{# fun unsafe score_remove_all as ^ { withScore* `Score' } -> `()' #}

-- | Retrieves the 'Timeline' for id inside score.
--
-- [@score@] a 'Score'
--
-- [@id@] the id of the timeline
--
-- [@Returns@] the 'Timeline'
--
-- * Since 0.6
--
{# fun unsafe score_get_timeline as ^
       { withScore* `Score', cIntConv `Word' } -> `Timeline' newTimeline* #}


-- | Retrieves a list of all the 'Timeline's managed by score.
--
-- [@score@] a 'Score'
--
-- [@Returns@] the list of 'Timeline's in the 'Score'
--
-- * Since 0.6
--
{# fun unsafe score_list_timelines as ^
       { withScore* `Score'} -> `[Timeline]' newTimelineList* #}

-- | Starts the score.
--
-- * Since 0.6
--
{# fun score_start as ^ { withScore* `Score' } -> `()' #}

-- | Pauses a playing score score.
--
-- * Since 0.6
--
{# fun score_pause as ^ { withScore* `Score' } -> `()' #}

-- | Stops and rewinds a playing 'Score' instance.
--
-- * Since 0.6
--
{# fun score_stop as ^ { withScore* `Score' } -> `()' #}

-- | Query state of a 'Score' instance.
--
-- [@score@] A 'Score'
--
-- [@Returns@] @True@ if score is currently playing
--
-- * Since 0.6
--
{# fun unsafe score_is_playing as ^ { withScore* `Score' } -> `Bool' #}


-- | Rewinds a 'Score' to its initial state.
--
-- * Since 0.6
--
{# fun unsafe score_rewind as ^ { withScore* `Score' } -> `()' #}


--onCompleted, afterCompleted :: Score -> IO () -> IO (ConnectId Score)


-- | The ::'completed' signal is emitted each time a 'Score' terminates.
--
-- * Since 0.6
--

-- onPaused, afterPaused :: Score -> IO () -> IO (ConnectId Score)


-- | The ::'paused' signal is emitted each time a 'Score' is paused.
--
-- * Since 0.6
--
-- paused :: Signal Score (IO ())



-- onStarted, afterStarted :: Score -> IO () -> IO (ConnectId Score)


instance Playable Score where
  start = scoreStart
  pause = scorePause
  stop = scoreStop
  started = Signal (connect_NONE__NONE "started")
  onStarted = connect_NONE__NONE "started" False
  afterStarted = connect_NONE__NONE "started" True
  completed = Signal (connect_NONE__NONE "completed")
  onCompleted = connect_NONE__NONE "completed" False
  afterCompleted = connect_NONE__NONE "completed" True
  paused = Signal (connect_NONE__NONE "paused")
  onPaused = connect_NONE__NONE "paused" False
  afterPaused = connect_NONE__NONE "paused" True


onTimelineCompleted, afterTimelineCompleted :: Score -> (Timeline -> IO ()) -> IO (ConnectId Score)
onTimelineCompleted = connect_OBJECT__NONE "timeline-completed" False
afterTimelineCompleted = connect_OBJECT__NONE "timeline-completed" True


-- | The ::'timelineCompleted' signal is emitted each time a timeline
--   inside a 'Score' terminates.
--
-- [@timeline@] the completed timeline
--
-- * Since 0.6
--
timelineCompleted :: Signal Score (Timeline -> IO ())
timelineCompleted = Signal (connect_OBJECT__NONE "timeline-completed")


onTimelineStarted, afterTimelineStarted :: Score -> (Timeline -> IO ()) -> IO (ConnectId Score)
onTimelineStarted = connect_OBJECT__NONE "timeline-started" False
afterTimelineStarted = connect_OBJECT__NONE "timeline-started" True


-- | The ::'timelineStarted' signal is emitted each time a new timeline
--   inside a 'Score' starts playing.
--
-- [@timeline@] the current timeline
--
-- * Since 0.6
--
timelineStarted :: Signal Score (Timeline -> IO ())
timelineStarted = Signal (connect_OBJECT__NONE "timeline-started")


