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

-- | Shader — Programmable pipeline abstraction
module Graphics.UI.Clutter.Shader (
-- * Description
-- | 'Shader' is an object providing an abstraction over the OpenGL
-- programmable pipeline. By using ClutterShaders is possible to
-- override the drawing pipeline by using small programs also known as
-- "shaders".
--
-- ClutterShader is available since Clutter 0.6
--
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


-- * Create a new 'Shader' instance.
--
-- [@Returns@] a new Shader.
--
-- * Since 0.6
--
{# fun unsafe shader_new as ^ { } -> `Shader' newShader* #}

-- | Checks whether shader is is currently compiled, linked and bound
--   to the GL context.
--
-- [@shader@] a 'Shader'
--
-- [@Returns@] @True@ if the shader is compiled, linked and ready for
-- use.
--
-- * Since 0.8
--
{# fun unsafe shader_is_compiled as ^ { withShader* `Shader' } -> `Bool' #}



-- | Enables a shader. This function will attempt to compile and link
--   the shader, if it isn't already.
--
-- When enabled is @False@ the default state of the GL pipeline will
-- be used instead.
--
-- [@shader@] a 'Shader'
--
-- [@enabled@] The new state of the shader.
--
-- * Since 0.6
--
{# fun unsafe shader_set_is_enabled as ^ { withShader* `Shader', `Bool' } -> `()' #}

-- | Checks whether shader is enabled.
--
-- [@shader@] a 'Shader'
--
-- [@Returns@] @True@ if the shader is enabled.
--
-- * Since 0.6
--
{# fun unsafe shader_get_is_enabled as ^ { withShader* `Shader' } -> `Bool' #}

shaderIsEnabled :: Attr Shader Bool
shaderIsEnabled = newAttr shaderGetIsEnabled shaderSetIsEnabled

