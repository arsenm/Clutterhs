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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}

#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.GValue (
                                 --GValueClass,
                                   withGValue
                                  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Animation #}

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


--this seems like it should have been done already.  The motivation is
--you don't need to do an explicit conversion / creation of a GValue
--when you try to use clutter_animatev and co
--I might do this. I might just not bother.
--I'm not sure it's worth it.
{-
class GValueClass a where
--  toGValue :: a -> IO GValue
  withGValue :: a -> (GValue -> IO b) -> IO b
  withGValue (GValue gval) = castPtr gval
-}

withGValue (GValue gval) = castPtr gval

