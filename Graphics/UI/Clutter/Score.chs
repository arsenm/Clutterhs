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
    scoreLoop
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe score_new as ^ {} -> `Score' newScore* #}

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

