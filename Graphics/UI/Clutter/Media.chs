-- -*-haskell-*-
--  Clutter Media
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Media (
                                  mediaSetUri,
                                  mediaGetUri,
                                  mediaUri,

                                  mediaSetPlaying,
                                  mediaGetPlaying,
                                  mediaPlaying,

                                  mediaSetProgress,
                                  mediaGetProgress,
                                  mediaProgress,

                                  mediaSetAudioVolume,
                                  mediaGetAudioVolume,
                                  mediaAudioVolume,

                                  mediaGetCanSeek,  --TODO: Read only attrs
                                --mediaCanSeek,
                                  mediaGetBufferFill,
                                --mediaBufferFill,
                                  mediaGetDuration,
                                --mediaDuration
                                  mediaSetFilename
                                --mediaFilename
                                 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe media_set_uri as ^ { withMedia* `Media', `String' } -> `()' #}
{# fun unsafe media_get_uri as ^ { withMedia* `Media' } -> `String' #}
mediaUri :: Attr Media String
mediaUri = newAttr mediaGetUri mediaSetUri

{# fun unsafe media_set_playing as ^ { withMedia* `Media', `Bool' } -> `()' #}
{# fun unsafe media_get_playing as ^ { withMedia* `Media' } -> `Bool' #}
mediaPlaying :: Attr Media Bool
mediaPlaying = newAttr mediaGetPlaying mediaSetPlaying

{# fun unsafe media_set_progress as ^ { withMedia* `Media', `Double' } -> `()' #}
{# fun unsafe media_get_progress as ^ { withMedia* `Media' } -> `Double' #}
mediaProgress :: Attr Media Double
mediaProgress = newAttr mediaGetProgress mediaSetProgress

{# fun unsafe media_set_audio_volume as ^ { withMedia* `Media', `Double' } -> `()' #}
{# fun unsafe media_get_audio_volume as ^ { withMedia* `Media' } -> `Double' #}
mediaAudioVolume :: Attr Media Double
mediaAudioVolume = newAttr mediaGetAudioVolume mediaSetAudioVolume

{# fun unsafe media_get_can_seek as ^ { withMedia* `Media' } -> `Bool' #}
{- TODO: Read only attrs
mediaCanSeek :: Attr Media Bool
mediaCanSeek = newAttr mediaGetPlaying mediaSetPlaying
-}
{# fun unsafe media_get_buffer_fill as ^ { withMedia* `Media' } -> `Double' #}
{# fun unsafe media_get_duration as ^ { withMedia* `Media' } -> `Double' #}

{# fun unsafe media_set_filename as ^ { withMedia* `Media', `String' } -> `()' #}

