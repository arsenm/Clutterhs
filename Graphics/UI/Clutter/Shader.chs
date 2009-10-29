-- -*-haskell-*-
--  Clutter Shader
--
--  Author : Matthew Arsenault
--
--  Created: 6 Oct 2009
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

module Graphics.UI.Clutter.Shader (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Shader'
-- @

-- * Constructors
  shaderNew,

-- * Methods
  shaderIsCompiled,
  shaderSetIsEnabled,
  shaderGetIsEnabled,
  shaderIsEnabled
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe shader_new as ^ { } -> `Shader' newShader* #}


{# fun unsafe shader_is_compiled as ^ { withShader* `Shader' } -> `Bool' #}

{# fun unsafe shader_set_is_enabled as ^ { withShader* `Shader', `Bool' } -> `()' #}
{# fun unsafe shader_get_is_enabled as ^ { withShader* `Shader' } -> `Bool' #}
shaderIsEnabled :: Attr Shader Bool
shaderIsEnabled = newAttr shaderGetIsEnabled shaderSetIsEnabled

