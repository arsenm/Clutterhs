-- -*-haskell-*-
--  Clutter Units
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


-- | Unit conversion — A logical distance unit
module Graphics.UI.Clutter.Units (
                                  unitsFromMm,
                                --unitsFromPt,
                                --unitsFromEm,
                                --unitsFromEmForFont,
                                --unitsFromPixels,
                                --unitsToPixels,
                                --unitsCopy,  --unnecessary
                                --unitsFree,
                                --unitsGetUnitType, --do these in record
                                --unitsGetUnitValue,
                                --unitsFromString,
                                --unitsToString  --instance for show?
                                --paramSpecUnits, --TODO: what is this?
                                --valueSetUnits,
                                --valueGetUnits
                                 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)

unitsFromMm = undefined

