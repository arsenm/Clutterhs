-- -*-haskell-*-
--  GValue
--
--  Author : Matthew Arsenault
--
--  Created: 25 Sep 2009
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances  #-}

#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.GValue (
                                   gValueInitSet,
                                   GValueClass,

                                   color,
                                   valueSetColor,
                                   valueGetColor
                                  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.External #}

import C2HS

import System.Glib.GObject
import System.Glib.GType
import System.Glib.GValue
import System.Glib.Attributes
import System.Glib.Properties

import System.Glib.GValueTypes
import Control.Arrow (second)

import qualified System.Glib.GTypeConstants as GType

import Control.Monad (liftM)

class GValueClass a where
    gValueInitSet :: GValue -> a -> IO ()

instance GValueClass Int where
    gValueInitSet gv val = valueInit gv GType.int >> valueSetInt gv val

instance GValueClass Double where
    gValueInitSet gv val = valueInit gv GType.double >> valueSetDouble gv val

instance GValueClass Float where
    gValueInitSet gv val = valueInit gv GType.float >> valueSetFloat gv val

instance GValueClass String where
    gValueInitSet gv val = valueInit gv GType.string >> valueSetString gv val

instance GValueClass Char where
    gValueInitSet gv val = valueInit gv GType.char >> valueSetChar gv val

instance GValueClass Word8 where
    gValueInitSet gv val = valueInit gv GType.uchar >> valueSetUChar gv val

instance GValueClass Bool where
    gValueInitSet gv val = valueInit gv GType.bool >> valueSetBool gv val

instance GValueClass Color where
    gValueInitSet gv val = valueInit gv color >> valueSetColor gv val

instance GValueClass GObject where
    gValueInitSet gv val = valueInit gv GType.object >> valueSetGObject gv val

--Color GValue
{# fun unsafe value_get_color as ^ { withGValue `GValue' } -> `Color' peek* #}
{# fun unsafe value_set_color as ^ { withGValue `GValue', withColor* `Color' } -> `()' #}

--constant
{# fun pure unsafe color_get_type as color { } -> `GType' cToEnum #}

