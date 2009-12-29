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
-- * Types
  Units,
  UnitType,

-- * Constructors
  unitsFromMm,
  unitsFromPt,
  unitsFromEm,
  unitsFromEmForFont,
  unitsFromPixels,

-- * Conversions
  unitsToPixels,
  unitsCopy,
  unitsGetUnitType,
  unitsGetUnitValue,
  unitsFromString,
  unitsToString
--paramSpecUnits,
--valueSetUnits,
--valueGetUnits
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)


-- | Stores a value in millimiters inside units
--
-- [@mm@] millimeters
--
-- [@Returns@] a 'Units'
--
-- * Since 1.0
--
{# fun unsafe units_from_mm as ^ { allocUnits- `Units' newUnits*, `Float' } -> `()' #}



-- | Stores a value in typographic points inside units
--
-- [@pt@] typographic points
--
-- [@Returns@] A 'Units'
--
-- * Since 1.0
--
{# fun unsafe units_from_pt as ^ { allocUnits- `Units' newUnits*, `Float' } -> `()' #}


-- | Stores a value in em inside units, using the default font name as
-- returned by 'backendGetFontName'
--
-- [@em@] em
--
-- [@Returns@] a 'Units'
--
-- * Since 1.0
--
{# fun unsafe units_from_em as ^ { allocUnits- `Units' newUnits*, `Float' } -> `()' #}


-- | Stores a value in em inside units using font_name
--
-- [@font_name@] the font name and size
--
-- [@em@] em
--
-- [@Returns@] a 'Units'
--
-- * Since 1.0
--
{# fun unsafe units_from_em_for_font as ^ { allocUnits- `Units' newUnits*, `String', `Float' } -> `()' #}


-- | Stores a value in pixels inside units
--
-- [@px@] pixels
--
-- [@Returns@] A 'Units'
--
-- * Since 1.0
--
{# fun unsafe units_from_pixels as ^ { allocUnits- `Units' newUnits*, `Int' } -> `()' #}



-- | Converts a value in 'Units' to pixels
--
-- [@units@] units to convert
--
-- [@Returns@] the value in pixels
--
-- * Since 1.0
--
{# fun unsafe units_to_pixels as ^ { withUnits* `Units' } -> `Float' #}



-- | Copies units
--
-- [@units@] the 'Units' to copy
--
-- [@Returns@] the newly created copy of a 'Units' structure.
--
-- * Since 1.0
--
{# fun unsafe units_copy as ^ { withUnits* `Units' } -> `Units' newCopiedUnits* #}


-- | Retrieves the unit type of the value stored inside units
--
-- [@units@] a 'Units'
--
-- [@Returns@] a unit type
--
-- * Since 1.0
--
{# fun unsafe units_get_unit_type as ^ { withUnits* `Units' } -> `UnitType' cToEnum #}



-- | Retrieves the value stored inside units
--
-- [@units@] a 'Units'
--
-- [@Returns@] the value stored inside a 'Units'
--
-- * Since 1.0
--
{# fun unsafe units_get_unit_value as ^ { withUnits* `Units' } -> `Float' #}


-- | Parses a value and updates units with it
--
-- A 'Units' expressed in string should match:
--
--  number: [0-9]
--  unit_value: <number>+
--  unit_name: px|pt|mm|em
--  units: <unit_value> <unit_name>
--
-- For instance, these are valid strings:
--
-- >  10 px
-- >  5.1 em
-- >  24 pt
-- >  12.6 mm
--
-- While these are not:
--
-- >  42 cats
-- >  omg!1!ponies
--
-- * Note
--
-- If no unit is specified, pixels are assumed.
--
-- [@units@] a 'Units'
--
-- [@str@] the string to convert
--
-- [@Returns@] @Just@ the units if the string was successfully parsed,
-- and @Nothing@ otherwise
--
-- * Since 1.0
--
unitsFromString :: String -> IO (Maybe Units)
unitsFromString str = withCString str $ \strPtr ->
                        allocUnits $ \unitPtr -> do
                            ret <- {# call unsafe units_from_string #} unitPtr strPtr
                            if (cToBool ret) == True
                               then newUnits unitPtr >>= return . Just
                               else return Prelude.Nothing

-- | Converts units into a string
--
-- See 'unitsFromString' for the units syntax and for examples of output
--
-- * Note
--
-- Fractional values are truncated to the second decimal position for
-- em and mm, and to the first decimal position for typographic
-- points. Pixels are integers.
--
-- [@units@] a 'Units'
--
-- [@Returns@] a string containing the encoded 'Units' value.
--
-- * Since 1.0
--
{# fun unsafe units_to_string as ^ { withUnits* `Units' } -> `String' peekNFreeString* #}

