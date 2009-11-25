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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}


-- | Media — An interface for controlling playback of media data
module Graphics.UI.Clutter.Media (
-- * Description
-- | 'Media' is an interface for controlling playback of media
--   sources.
--
-- Clutter core does not provide an implementation of this interface,
-- but other integration libraries like Clutter-GStreamer implement it
-- to offer a uniform API for applications.
--
-- 'Media' is available since Clutter 0.2
--

-- * Class Hierarchy
-- |
-- @
-- |  'GInterface'
-- |   +----'Media'
-- @

-- * Types
  Media,
  MediaClass,

-- * Methods
  mediaSetUri,
  mediaGetUri,
  mediaSetPlaying,
  mediaGetPlaying,
  mediaSetProgress,
  mediaGetProgress,
  mediaSetAudioVolume,
  mediaGetAudioVolume,
  mediaGetCanSeek,
  mediaGetBufferFill,
  mediaGetDuration,
  mediaSetFilename,

-- * Attributes
  mediaAudioVolume,
  mediaBufferFill,
  mediaCanSeek,
  mediaDuration,
  mediaPlaying,
  mediaProgress,
  mediaUri,

-- * Signals
  onEos,
  afterEos,
  eos,
  onError,
  afterError,
  error
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}
{# import Graphics.UI.Clutter.Signals #}

import Prelude hiding (error)
import C2HS
import System.Glib.GError
import System.Glib.Attributes


-- | Sets the URI of media to uri.
--
-- [@media@] a Media
--
-- [@uri@] the URI of the media stream
--
-- * Since 0.2
--
{# fun unsafe media_set_uri as ^ `(MediaClass m)' => { withMediaClass* `m', `String' } -> `()' #}

-- | Retrieves the URI from media.
--
-- [@media@] a Media
--
-- [@Returns@] the URI of the media stream
--
-- * Since 0.2
--
{# fun unsafe media_get_uri as ^
       `(MediaClass m)' => { withMediaClass* `m' } -> `String' peekNFreeString* #}


-- | Starts or stops playing of media.
--
-- [@media@] a Media
--
-- [@playing@] @True@ to start playing
--
-- * Since 0.2
--
{# fun unsafe media_set_playing as ^ `(MediaClass m)' => { withMediaClass* `m', `Bool' } -> `()' #}

-- | Retrieves the playing status of media.
--
-- [@media@] A Media object
--
-- [@Returns@] @True@ if playing, @False@ if stopped.
--
-- * Since 0.2
--
{# fun unsafe media_get_playing as ^ `(MediaClass m)' => { withMediaClass* `m' } -> `Bool' #}

-- | Sets the playback progress of media. The progress is a normalized
--   value between 0.0 (begin) and 1.0 (end).
--
-- [@media@] a Media
--
-- [@progress@] the progress of the playback, between 0.0 and 1.0
--
-- * Since 1.0
--
{# fun unsafe media_set_progress as ^
       `(MediaClass m)' => { withMediaClass* `m', `Double' } -> `()' #}

-- | Retrieves the playback progress of media.
--
-- [@media@] a Media
--
-- [@Returns@] the playback progress, between 0.0 and 1.0
--
-- * Since 1.0
--
{# fun unsafe media_get_progress as ^
       `(MediaClass m)' => { withMediaClass* `m' } -> `Double' #}


-- | Sets the playback volume of media to volume.
--
-- [@media@] a Media
--
-- [@volume@] the volume as a double between 0.0 and 1.0
--
-- * Since 1.0
--
{# fun unsafe media_set_audio_volume as ^
       `(MediaClass m)' => { withMediaClass* `m', `Double' } -> `()' #}

-- | Retrieves the playback volume of media.
--
-- [@media@] a Media
--
-- [@Returns@] The playback volume between 0.0 and 1.0
--
-- * Since 1.0
{# fun unsafe media_get_audio_volume as ^
       `(MediaClass m)' => { withMediaClass* `m' } -> `Double' #}

-- | Retrieves whether media is seekable or not.
--
-- [@media@] a Media
--
-- [@Returns@] @True@ if media can seek, @False@ otherwise.
--
-- * Since 0.2
--
{# fun unsafe media_get_can_seek as ^ `(MediaClass m)' => { withMediaClass* `m' } -> `Bool' #}


-- | Retrieves the amount of the stream that is buffered.
--
-- [@media@] a Media
--
-- [@Returns@] the fill level, between 0.0 and 1.0
--
-- * Since 1.0
--
{# fun unsafe media_get_buffer_fill as ^
       `(MediaClass m)' => { withMediaClass* `m' } -> `Double' #}

-- | Retrieves the duration of the media stream that media represents.
--
-- [@media@] a Media
--
-- [@Returns@] the duration of the media stream, in seconds
--
-- * Since 0.2
--
{# fun unsafe media_get_duration as ^
       `(MediaClass m)' => { withMediaClass* `m' } -> `Double' #}

-- | Sets the source of media using a file path.
--
-- [@media@] a Media
--
-- [@filename@] A filename
--
-- * Since 0.2
--
{# fun unsafe media_set_filename as ^
       `(MediaClass m)' => { withMediaClass* `m', `String' } -> `()' #}


-- attributes

mediaAudioVolume :: (MediaClass media) => Attr media Double
mediaAudioVolume = newNamedAttr "audio-volume" mediaGetAudioVolume mediaSetAudioVolume

mediaBufferFill :: (MediaClass media) => ReadAttr media Double
mediaBufferFill = readNamedAttr "buffer-fill" mediaGetBufferFill

mediaCanSeek :: (MediaClass media) => ReadAttr media Bool
mediaCanSeek = readAttr mediaGetCanSeek

mediaDuration :: (MediaClass media) => ReadAttr media Double
mediaDuration = readNamedAttr "duration" mediaGetDuration

mediaPlaying :: (MediaClass media) => Attr media Bool
mediaPlaying = newNamedAttr "playing" mediaGetPlaying mediaSetPlaying

mediaProgress :: (MediaClass media) => Attr media Double
mediaProgress = newNamedAttr "progress" mediaGetProgress mediaSetProgress

mediaUri :: (MediaClass media) => Attr media String
mediaUri = newNamedAttr "uri" mediaGetUri mediaSetUri


-- signals

onEos, afterEos :: (MediaClass media) => media -> IO () -> IO (ConnectId media)
onEos = connect_NONE__NONE "eos" False
afterEos = connect_NONE__NONE "eos" True

-- | The ::eos signal is emitted each time the media stream ends.
--
eos :: (MediaClass media) => Signal media (IO ())
eos = Signal (connect_NONE__NONE "eos")

--CHECKME:checky
onError, afterError :: Timeline -> (GError -> IO ()) -> IO (ConnectId Timeline)
onError = connect_BOXED__NONE "error" peek False
afterError = connect_BOXED__NONE "error" peek True

-- | The ::error signal is emitted each time an error occurred.
--
-- [@error@] : the GError
--
-- * Since 0.2
--
error :: Signal Timeline (GError -> IO ())
error = Signal (connect_BOXED__NONE "error" peek)

