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
                                  colorFromString
--                                  colorNew,
--                                  colorFree,
--                                  colorCopy
                                 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Data.Word
import System.Glib.GType (GType, typeInstanceIsA)
import System.Glib.GObject

--FIXME: Check this for ok unsafePerformIO. Ceiling cat is watching.
colorFromString::String -> Maybe Color
colorFromString name = unsafePerformIO $ withCString name $ \cstr ->
                       alloca $ \colptr -> return $
                       if cToBool ({# call pure unsafe color_from_string #} colptr cstr)
                         then Just (unsafePerformIO (peek colptr))
                         else Prelude.Nothing

{-
--colorFromString::String -> Maybe Color
colorFromString name = do
  strptr <- newCString name
  colptr <- malloc :: IO (Ptr Color)
  succ <- {# call unsafe color_from_string #} (castPtr colptr) strptr
  let a = if cToBool succ
            then Just (unsafePerformIO (peek colptr))
            else Nothing
--  seq a (return ())
  free colptr
  free strptr
  return a
-}

--{#fun unsafe color_new
--      {withCUChar* `s', withCUChar* `s'} -> `Color' Color #}
--{#fun color_new { r b g a } -> `Color' Color #}

--{#fun color_new as colorNew { `CUChar', `CUChar', `CUChar', `CUChar'} -> `Color' Color #}

--addForeignPtrFinalizer ??

--colorCopy :: Color -> IO Color
--colorCopy = undefined


