-- -*-haskell-*-
--  Clutter Interval
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

module Graphics.UI.Clutter.Interval (
                                     intervalNew
                                   --intervalNewWithValues,
                                   --intervalClone,
                                   --intervalGetValueType,
                                   --intervalSetInitialValue,
                                   --intervalGetInitialValue,
                                   --intervalInitialValue,
                                   --intervalPeekInitalValue,
                                   --intervalSetFinalValue,
                                   --intervalGetFinalValue,
                                   --intervalFinalValue,
                                   --intervalPeekFinalValue,
                                   --intervalSetInterval,
                                   --intervalGetInterval,
                                   --intervalInterval,
                                   --intervalComputeValue,
                                   --intervalValidate,
                                   --intervalRegisterProgressFunc
                                    ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import System.Glib.GObject

intervalNew = error "ClutterInterval unimplemented"

--this seems to replace intervalNew for language bindings so rename it?
--intervalNewWithValues :: (
--intervalNewWithValues initial final

