-- -*-haskell-*-
--  Clutter Color
--
--  Author : Matthew Arsenault
--
--  Created: 4 Sep 2009
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

module Graphics.UI.Clutter.Color (
                                  colorNew,
                                  colorCopy
                                 ) where

{#import Graphics.UI.Clutter.Types#}

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr, unsafeForeignPtrToPtr)
import Foreign.C.Types (CULong, CUInt, CUChar)
import Foreign.Ptr (Ptr)
import Data.Word
import System.Glib.GType (GType, typeInstanceIsA)
import System.Glib.GObject

type GUInt8 = {#type guint8#}
-- | Creates a new color
--
colorNew :: GUInt8 -> GUInt8 -> GUInt8 -> GUInt8 -> IO (Ptr ClutterColor)
colorNew r b g a = {# call unsafe color_new #} r b g a
--{#fun unsafe color_new
--      `(RString s)' =>
--      {withRString* `s'      ,
--       withRString* `s'    } -> `ClutterColor'#}
--{#fun color_new { r b g a } -> `ClutterColor' ClutterColor #}

--{#fun color_new as colorNew { `CUChar', `CUChar', `CUChar', `CUChar'} -> `ClutterColor' ClutterColor #}


colorCopy :: ClutterColor -> IO ClutterColor
colorCopy = undefined


