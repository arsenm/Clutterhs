-- -*-haskell-*-
--  Clutter Scriptable
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

module Graphics.UI.Clutter.Scriptable (
-- |
-- @
-- |  'GInterface'
-- |   +----'Scriptable'
-- @

-- * Types
  ScriptableClass,

-- * Methods
  scriptableSetId,
  scriptableGetId,

--scriptableParseCustomNode,
--scriptableSetCustomProperty

-- * Attributes
  scriptableId
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.Attributes

{# fun unsafe scriptable_get_id as ^ `(ScriptableClass o)' => { withScriptableClass* `o'} -> `String' #}
{# fun unsafe scriptable_set_id as ^ `(ScriptableClass o)' => { withScriptableClass* `o', `String'} -> `()' #}
scriptableId :: (ScriptableClass self) => Attr self String
scriptableId = newAttr scriptableGetId scriptableSetId


