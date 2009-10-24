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
{-# LANGUAGE ForeignFunctionInterface,
             TypeSynonymInstances,
             UndecidableInstances,
             IncoherentInstances,
             OverlappingInstances #-}

#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

--TODO: Lots of stuff here should be private

module Graphics.UI.Clutter.GValue (
                                   gValueInitSet,
                                   GValueClass,
                                   GValueArg(..),
                                   GValueArgPtr,
                                   withGValueArg,
                                   unsetOneGVal,

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

--CHECKME: Should this be private?
data GValueArg = UChar Char
               | UString String
               | UUChar Word8
               | UInteger Int
               | UFloat Float
               | UDouble Double
               | UColor Color
               | UGObject GObject
           --- | UFunc (Actor -> IO ()) Actor

--CHECKME: Referencing here?
withGValueArg :: (GValueClass arg) => arg -> (GValueArg -> IO a) -> IO a
withGValueArg arg act = act (toGValueArg arg)

{# pointer *GValue as GValueArgPtr -> GValueArg #}

instance Storable GValueArg where
    sizeOf _ = {# sizeof GValue #}
    alignment _ = alignment (undefined :: GType)
    peek _ = error "peek undefined for GValueArg"
    poke p ut = let gv = GValue (castPtr p) --FIXME: This castPtr = badnews bears? Why needed?
                in do
                {# set GValue->g_type #} p (0 :: GType)
                case ut of
                --FIXME: Really actually want char /uchar or Int8/Word8?
                  (UInteger val) -> gValueInitSet gv val
                  (UDouble val) ->  gValueInitSet gv val
                  (UFloat val) -> gValueInitSet gv val
                  (UString val) ->  gValueInitSet gv val
                  (UChar val) ->  gValueInitSet gv val
                  (UUChar val) -> gValueInitSet gv val
                  (UColor val) -> gValueInitSet gv val
                  (UGObject val) -> gValueInitSet gv val

unsetOneGVal ::  GValueArgPtr -> Int -> IO GValueArgPtr
unsetOneGVal i u = {#call unsafe g_value_unset#} i >> return (advancePtr i 1)

class IntervalArg a where
    toGValueArg :: a -> GValueArg

instance IntervalArg String where
    toGValueArg = UString
instance IntervalArg Int where
    toGValueArg = UInteger
instance IntervalArg Float where
    toGValueArg = UFloat
instance IntervalArg Color where
    toGValueArg = UColor
instance IntervalArg Double where
    toGValueArg = UDouble
instance IntervalArg Word8 where
    toGValueArg = UUChar



--CHECKME: UndecidableInstances and IncoherentInstances needed for this. Check that it's ok and works
--when I was pairing, needed OverlappingInstances
instance (GObjectClass obj) => IntervalArg obj where
    toGValueArg = UGObject . toGObject


