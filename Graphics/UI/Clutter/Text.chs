-- -*-haskell-*-
--  Clutter Text
--
--  Author : Matthew Arsenault
--
--  Created: 17 Sep 2009
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

module Graphics.UI.Clutter.Text (
                                 textNew,
--                                 textNewFull,
                                 textNewWithText,
                                 textGetColor,
                                 textSetColor,
                                 textColor
                                ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes
import Control.Monad (liftM)

{# fun unsafe text_new as ^ {} -> `Text' newText* #}
{#fun unsafe text_new_with_text as ^ { `String', `String' } -> `Text' newText* #}

{# fun unsafe text_get_color as ^ {withText* `Text', alloca- `Color' peek* } -> `()' #}
{# fun unsafe text_set_color as ^ {withText* `Text', withColor* `Color'} -> `()' #}
textColor :: Attr Text Color
textColor = newAttr textGetColor textSetColor

