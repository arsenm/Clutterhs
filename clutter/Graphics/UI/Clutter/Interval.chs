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
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Interval'
-- @

-- * Constructors
  intervalNew
--intervalNewWithValues,
--intervalClone,

-- * Methods

--intervalGetValueType,
--intervalSetInitialValue,
--intervalGetInitialValue,

--intervalPeekInitalValue,
--intervalSetFinalValue,
--intervalGetFinalValue,

--intervalPeekFinalValue,
--intervalSetInterval,
--intervalGetInterval,

--intervalComputeValue,
--intervalValidate,
--intervalRegisterProgressFunc

-- * Attributes
--intervalInitialValue,
--intervalFinalValue,
--intervalInterval,
) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import System.Glib.GType
import qualified System.Glib.GTypeConstants as GType

intervalNew = error "ClutterInterval unimplemented"

{-
intervalNewWithValues :: (GValueArgClass arg) => arg -> arg -> IO Interval
intervalNewWithValues initial final = let func = {# call unsafe interval_new_with_values #}
                                          gtype = gValueArgGType initial
                                      in withGValueArg initial $ \argptr1 ->
                                          withGValueArg final $ \argptr2 ->
                                            newInterval =<< func gtype argptr1 argptr2
-}
{# fun unsafe interval_clone as ^
       { withInterval* `Interval' } -> `Interval' newInterval* #}

