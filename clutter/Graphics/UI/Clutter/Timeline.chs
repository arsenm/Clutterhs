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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- | Timeline — A class for time-based events
module Graphics.UI.Clutter.Timeline (
-- * Description
-- | 'Timeline' is a base class for managing time based events such as animations.
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Timeline'
-- @

-- * Types
  Timeline,
  TimelineClass,
  TimelineDirection(..),

-- * Constructors
  timelineNew,
  timelineClone,

-- * Methods
  timelineSetDuration,
  timelineGetDuration,

  timelineSetLoop,
  timelineGetLoop,

  timelineSetDelay,
  timelineGetDelay,

  timelineSetDirection,
  timelineGetDirection,

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
  timelineListMarkers,
  timelineRemoveMarker,
  timelineAdvanceToMarker,

-- * Attributes
  timelineDuration,
  timelineLoop,
  timelineDelay,
  timelineDirection,

-- * Signals
  onCompleted,
  afterCompleted,
  completed,

  onMarkerReached,
  afterMarkerReached,
  markerReached,

  onNewFrame,
  afterNewFrame,
  newFrame,

  onPaused,
  afterPaused,
  paused,

  onStarted,
  afterStarted,
  started
  ) where

{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.Signals


-- | Creates a new 'Timeline' with a duration of msecs.
--
-- [@msecs@] Duration of the timeline in milliseconds
--
-- [@Returns@] the newly created 'Timeline' instance
--
-- * Since 0.6
--
{# fun unsafe timeline_new as ^ { cIntConv `Word' } -> `Timeline' newTimeline* #}

-- | Create a new 'Timeline' instance which has property values
--   matching that of supplied timeline. The cloned timeline will not
--   be started and will not be positioned to the current position of
--   timeline: you will have to start it with 'timelineStart'.
--
-- [@timeline@] 'Timeline' to duplicate.
--
-- [@Returns@] a new 'Timeline', cloned from timeline
--
-- * Since 0.4
--
{# fun unsafe timeline_clone as ^ { withTimeline* `Timeline'} -> `Timeline' newTimeline* #}

-- | Sets the duration of the timeline, in milliseconds. The speed of
--   the timeline depends on the ClutterTimeline:fps setting.
--
-- [@timeline@] a 'Timeline'
--
-- [@msecs@] duration of the timeline in milliseconds
--
-- * Since 0.6
--
{# fun unsafe timeline_set_duration as ^ { withTimeline* `Timeline', cIntConv `Word' } -> `()' #}

-- | Retrieves the duration of a 'Timeline' in milliseconds. See
--   'timelineSetDuration'.
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] the duration of the timeline, in milliseconds.
--
-- * Since 0.6
--
{# fun unsafe timeline_get_duration as ^ { withTimeline* `Timeline' } -> `Word' cIntConv #}


-- | Sets whether timeline should loop.
--
-- [@timeline@] a 'Timeline'
--
-- [@loop@] @True@ for enable looping
--
{# fun unsafe timeline_set_loop as ^ { withTimeline* `Timeline', `Bool' } -> `()' #}


-- | Gets whether timeline is looping
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] @True@ if the timeline is looping
--
{# fun unsafe timeline_get_loop as ^ { withTimeline* `Timeline' } -> `Bool' #}

-- | Sets the delay, in milliseconds, before timeline should start.
--
-- [@timeline@] a 'Timeline'
--
-- [@msecs@] delay in milliseconds
--
-- * Since 0.4
--
{# fun unsafe timeline_set_delay as ^ { withTimeline* `Timeline', cIntConv `Word' } -> `()' #}

-- | Retrieves the delay set using 'timelineSetDelay'.
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] the delay in milliseconds.
--
-- * Since 0.4
--
{# fun unsafe timeline_get_delay as ^ { withTimeline* `Timeline' } -> `Word' cIntConv #}

-- | Sets the direction of timeline, either TimelineForward or
--   TimelineBackward.
--
-- [@timeline@] a 'Timeline'
--
-- [@direction@] the direction of the timeline
--
-- * Since 0.6
--
{# fun unsafe timeline_set_direction as ^
       { withTimeline* `Timeline', cFromEnum `TimelineDirection' } -> `()' #}

-- | Retrieves the direction of the timeline set with
--   'TimelineSetDirection'.
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] the direction of the timeline
--
-- * Since 0.6
--
{# fun unsafe timeline_get_direction as ^
       { withTimeline* `Timeline' } -> `TimelineDirection' cToEnum #}

-- | Starts the 'Timeline' playing.
{# fun timeline_start as ^ { withTimeline* `Timeline' } -> `()' #}

-- | Pauses the 'Timeline' on current frame
{# fun timeline_pause as ^ { withTimeline* `Timeline' } -> `()' #}

-- | Stops the 'Timeline' and moves to frame 0
{# fun timeline_stop as ^ { withTimeline* `Timeline' } -> `()' #}

-- | Rewinds ClutterTimeline to the first frame if its direction is
--   TimelineForward and the last frame if it is TimelineBackward.
{# fun timeline_rewind as ^ { withTimeline* `Timeline' } -> `()' #}

-- | Advance timeline by the requested time in milliseconds
--
-- [@timeline@] A 'Timeline'
--
-- [@msecs@] Amount of time to skip
--
{# fun timeline_skip as ^ { withTimeline* `Timeline', cIntConv `Word' } -> `()' #}

--CHECKME: Since it's skipping the given time, but does it skip others? mark unsafe or not?
-- | Advance timeline to the requested point. The point is given as a
--   time in milliseconds since the timeline started.
--
-- * Note
--
-- The timeline will not emit the "new-frame" signal for the given
-- time. The first ::new-frame signal after the call to
-- 'timelineAdvance' will be emit the skipped markers.
--
-- [@timeline@] A 'Timeline'
--
-- [@msecs@] Time to advance to
--
{# fun unsafe timeline_advance as ^ { withTimeline* `Timeline', cIntConv `Word' } -> `()' #}


-- | Request the current time position of the timeline.
--
-- [@timeline@] A 'Timeline'
--
-- [@Returns@] current elapsed time in milliseconds.
--
{# fun unsafe timeline_get_elapsed_time as ^ { withTimeline* `Timeline' } -> `Word' cIntConv #}


--TODO: Links to all the signals in the doc
-- | Retrieves the amount of time elapsed since the last 'Timeline'::new-frame signal.
--
-- This function is only useful inside handlers for the ::new-frame
-- signal, and its behaviour is undefined if the timeline is not
-- playing.
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] the amount of time in milliseconds elapsed since the last frame
--
-- * Since 0.6
--
{# fun unsafe timeline_get_delta as ^ { withTimeline* `Timeline' } -> `Word' cIntConv #}


-- | The position of the timeline in a [0, 1] interval.
--
-- [@timeline@] a 'Timeline'
--
-- [@Returns@] the position of the timeline.
--
-- * Since 0.6
--
{# fun unsafe timeline_get_progress as ^ { withTimeline* `Timeline' } -> `Double' #}

-- | Queries state of a 'Timeline'.
--
-- [@timeline@] A 'Timeline'
--
-- [@Returns@] @True@ if timeline is currently playing
--
{# fun unsafe timeline_is_playing as ^ { withTimeline* `Timeline' } -> `Bool' #}

-- | Adds a named marker that will be hit when the timeline has been
--   running for msecs milliseconds. Markers are unique string
--   identifiers for a given time. Once timeline reaches msecs, it
--   will emit a ::marker-reached signal for each marker attached to
--   that time.
--
-- A marker can be removed with 'timelineRemoveMarker'. The timeline
-- can be advanced to a marker using 'timelineAdvanceToMarker'.
--
-- [@timeline@] a 'Timeline'
--
-- [@marker_name@] the unique name for this marker
--
-- [@msecs@] position of the marker in milliseconds
--
-- * Since 0.8
--
{# fun unsafe timeline_add_marker_at_time as ^
       { withTimeline* `Timeline', `String', cIntConv `Word' } -> `()' #}


-- | Checks whether timeline has a marker set with the given name.
--
-- [@timeline@] a 'Timeline'
--
-- [@marker_name@] the name of the marker
--
-- [@Returns@] @True@ if the marker was found
--
-- * Since 0.8
--
{# fun unsafe timeline_has_marker as ^ { withTimeline* `Timeline', `String' } -> `Bool' #}

--CHECME: Unicode?
-- | Retrieves the list of markers at time msecs. If frame_num is a
--   negative integer, all the markers attached to timeline will be
--   returned.
--
-- [@timeline@] a 'Timeline'
--
-- [@msecs@] the time to check, or -1
--
-- [@Returns@] A list of markers returned
--
-- * Since 0.8
--
timelineListMarkers :: Timeline -> Int -> IO [String]
timelineListMarkers tml time = withTimeline tml $ \tmlptr ->
                               alloca $ \intptr -> do
                               strArrayPtr <- {# call unsafe timeline_list_markers #} tmlptr (cIntConv time) intptr
                               num <- peek intptr
                               strPtrList <- peekArray (cIntConv num) strArrayPtr
                               mapM peekNFreeString strPtrList

-- | Removes marker_name, if found, from timeline.
--
-- [@timeline@] a 'Timeline'
--
-- [@marker_name@] the name of the marker to remove
--
-- * Since 0.8
--
{# fun unsafe timeline_remove_marker as ^ { withTimeline* `Timeline', `String' } -> `()' #}

-- | Advances timeline to the time of the given marker_name.
--
-- * Note
--
-- Like 'timelineAdvance', this function will not emit the "new-frame"
-- for the time where marker_name is set, nor it will emit
-- "marker-reached" for marker_name.
--
-- [@timeline@] a 'Timeline'
--
-- [@marker_name@] the name of the marker
--
-- * Since 0.8
--
{# fun unsafe timeline_advance_to_marker as ^ { withTimeline* `Timeline', `String' } -> `()' #}


-- | Duration of the timeline in milliseconds, depending on the
--   'Timeline':fps value.
--
-- Default value: 1000
--
-- * Since 0.6
--
timelineDuration :: Attr Timeline Word
timelineDuration = newNamedAttr "duration" timelineGetDuration timelineSetDuration


-- | Whether the timeline should automatically rewind and restart.
--
-- Default value: @False@
--
timelineLoop :: Attr Timeline Bool
timelineLoop = newNamedAttr "loop" timelineGetLoop timelineSetLoop


-- | A delay, in milliseconds, that should be observed by the timeline
--   before actually starting.
--
-- Default value: 0
--
-- * Since 0.4
--
timelineDelay :: Attr Timeline Word
timelineDelay = newNamedAttr "delay" timelineGetDelay timelineSetDelay


-- | The direction of the timeline, either TimelineForward
--   or TimelineBackward.
--
-- Default value: TimelineForward
--
-- * Since 0.6
--
timelineDirection :: Attr Timeline TimelineDirection
timelineDirection = newNamedAttr "direction" timelineGetDirection timelineSetDirection



--TODO: Check these

--onCompleted, afterCompleted :: Timeline -> IO () -> IO (ConnectId Timeline)

-- | The ::completed signal is emitted when the timeline reaches the
--   number of frames specified by the 'Timeline':num-frames
--   property.
--completed :: Signal Timeline (IO ())

onMarkerReached, afterMarkerReached :: Timeline -> (String -> Word -> IO ()) -> IO (ConnectId Timeline)
onMarkerReached = connect_STRING_WORD__NONE "marker-reached" False
afterMarkerReached = connect_STRING_WORD__NONE "marker-reached" True

--TODO: Code part of this doc
-- | The ::marker-reached signal is emitted each time a timeline
--   reaches a marker set with 'timelineAddMarkerAtTime'. This signal
--   is detailed with the name of the marker as well, so it is
--   possible to connect a callback to the ::marker-reached signal for
--   a specific marker with:
--
-- TODO: The example
--
-- In the example, the first callback will be invoked for both the
-- \"foo\" and \"bar\" marker, while the second and third callbacks
-- will be invoked for the \"foo\" or \"bar\" markers, respectively.
--
-- [@timeline@] the 'Timeline' which received the signal
--
-- [@marker_name@] the name of the marker reached
--
-- [@msecs@] the elapsed time
--
-- * Since 0.8
--
markerReached :: Signal Timeline (String -> Word -> IO ())
markerReached = Signal (connect_STRING_WORD__NONE "marker-reached")

onNewFrame, afterNewFrame :: Timeline -> (Int -> IO ()) -> IO (ConnectId Timeline)
onNewFrame = connect_INT__NONE "new-frame" False
afterNewFrame = connect_INT__NONE "new-frame" True


-- | The ::new-frame signal is emitted for each timeline running
--   timeline before a new frame is drawn to give animations a chance
--   to update the scene.
--
-- [@timeline@] the timeline which received the signal
--
-- [@msecs@] the elapsed time between 0 and duration
--
newFrame :: Signal Timeline (Int -> IO ())
newFrame = Signal (connect_INT__NONE "new-frame")


instance Playable Timeline where
  started = Signal (connect_NONE__NONE "started")
  onStarted = connect_NONE__NONE "started" False
  afterStarted = connect_NONE__NONE "started" True
  completed = Signal (connect_NONE__NONE "completed")
  onCompleted = connect_NONE__NONE "completed" False
  afterCompleted = connect_NONE__NONE "completed" True
  paused = Signal (connect_NONE__NONE "paused")
  onPaused = connect_NONE__NONE "paused" False
  afterPaused = connect_NONE__NONE "paused" True


-- | The ::paused signal is emitted when 'timelinePause' is invoked.
-- paused :: Signal Timeline (IO ())


-- onStarted, afterStarted :: Timeline -> IO () -> IO (ConnectId Timeline)



-- | The ::started signal is emitted when the timeline starts its
--   run. This might be as soon as 'timelineStart' is invoked or after
--   the delay set in the 'Timeline':delay property has expired.
--started :: Signal Timeline (IO ())


