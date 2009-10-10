-- -*-haskell-*-
--  Stuff that most likely belongs in Gtk2hs, or are broken or something
--
--  Author : Matthew Arsenault
--
--  Created: 10 Oct 2009
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
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}
{# context lib="glib" prefix="g" #}

module Graphics.UI.Clutter.External (
                                     valueSetUchar,
                                     valueGetUchar,
                                     valueSetChar,
                                     valueGetChar
                                    ) where

import C2HS
import Control.Monad (liftM)
import System.Glib.GType
import System.Glib.GValue (GValue(GValue))


--valueSetInt :: GValue -> Int -> IO ()
--valueSetInt (GValue gvalue) value =
--  {# call unsafe value_set_int #} gvalue (fromIntegral value)

withGValue (GValue gval) = castPtr gval

--{# fun unsafe value_set_int as ^ { withGValue `GValue', `Int' } -> `()' #}

--TODO: loltypes. FIXFIXFIX.
--More importantly. This is commented out in c2hs.
--Lack of intconv?
{# fun unsafe value_get_char as ^ { withGValue `GValue' } -> `Int8' #}
{# fun unsafe value_set_char as ^ { withGValue `GValue', `Int8' } -> `()' #}

{# fun unsafe value_set_uchar as ^ { withGValue `GValue', `Word8' } -> `()' #}
{# fun unsafe value_get_uchar as ^ { withGValue `GValue' } -> `Word8' #}


