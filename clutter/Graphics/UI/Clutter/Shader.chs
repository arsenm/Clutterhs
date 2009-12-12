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

-- * Types
  Shader,
  ShaderClass,
  ShaderError(..),

-- * Constructors
  shaderNew,

-- * Methods
  shaderSetVertexSource,
  shaderGetVertexSource,

  shaderSetFragmentSource,
  shaderGetFragmentSource,

  shaderCompile,
  shaderRelease,

  shaderIsCompiled,

  shaderSetIsEnabled,
  shaderGetIsEnabled,

--shaderSetUniform,
--shaderGetCoglProgram,
--shaderGetCoglFragmentShader,
--shaderGetCoglVertexShader,

--valueHoldsShaderFloat,

--valueSetShaderFloat,
--valueGetShaderFloat,
--valueShaderFloat,

--valueHoldsShaderInt,
--valueSetShaderInt,
--valueGetShaderInt,
--valueShaderInt,

--valueHoldsShaderMatrix,
--valueSetShaderMatrix,
--valueGetShaderMatrix,
--valueShaderMatrix

-- * Attributes
  shaderCompiled,
  shaderEnabled,
  shaderFragmentSource,
  shaderVertexSource
  ) where

{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GError


-- | Create a new 'Shader' instance.
--
-- [@Returns@] a new Shader.
--
-- * Since 0.6
--
{# fun unsafe shader_new as ^ { } -> `Shader' newShader* #}


-- | Sets the GLSL source code to be used by a 'Shader' for the vertex
--   program.
--
-- [@shader@] a 'Shader'
--
-- [@data@] GLSL source code.
--
-- * Since 0.6
--
shaderSetVertexSource :: Shader -> String -> IO ()
shaderSetVertexSource shader str= let func = {# call unsafe shader_set_vertex_source #}
                                  in withShader shader $ \shdPtr ->
                                       withCStringLen str $ \(strPtr, len) ->
                                         func shdPtr strPtr (cIntConv len)

-- | Query the current GLSL vertex source set on shader.
--
-- [@shader@] a 'Shader'
--
-- [@Returns@] @Just@ the source of the vertex shader for this
-- 'Shader' object or @Nothing@.
--
-- * Since 0.6
--
{# fun unsafe shader_get_vertex_source as ^ { withShader* `Shader' } -> `Maybe String' maybeString* #}


-- | Query the current GLSL fragment source set on shader.
--
-- [@shader@] a 'Shader'
--
-- [@Returns@] the source of the fragment shader for this 'Shader'
-- object or NULL. The returned string is owned by the shader object
-- and should never be modified or freed
--
-- * Since 0.6
--
shaderSetFragmentSource :: Shader -> String -> IO ()
shaderSetFragmentSource shader str= let func = {# call unsafe shader_set_fragment_source #}
                                    in withShader shader $ \shdPtr ->
                                         withCStringLen str $ \(strPtr, len) ->
                                           func shdPtr strPtr (cIntConv len)

-- | Query the current GLSL fragment source set on shader.
--
-- [@shader@] a 'Shader'
--
-- [@Returns@] @Just@ the source of the fragment shader for this
-- 'Shader' object or @Nothing@.
--
-- * Since 0.6
--
{# fun unsafe shader_get_fragment_source as ^ { withShader* `Shader' } -> `Maybe String' maybeString* #}

--TODO: Fix this description of GError
-- | Compiles and links GLSL sources set for vertex and fragment
--   shaders for a 'Shader'. If the compilation fails a GError
--   exception will be thrown containing the errors from the compiler,
--   if any.
--
-- [@shader@] a 'Shader'
--
-- [@Returns@] returns @True@ if the shader was succesfully compiled.
--
-- * Since 0.8
--
shaderCompile :: Shader -> IO Bool
shaderCompile shader = withShader shader $ \sPtr ->
                         propagateGError $ \gerrorPtr ->
                             liftM cToBool $ {# call unsafe shader_compile #} sPtr gerrorPtr


-- | Frees up any GL context resources held by the shader.
--
-- [@shader@] a 'Shader'
--
-- * Since 0.6
--
{# fun unsafe shader_release as ^ { withShader* `Shader' } -> `()' #}

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

--TODO: GValue
--{# fun unsafe shader_set_uniform as ^ { withShader* `Shader', `String',  } -> `()' #}

--{# fun unsafe shader_get_cogl_program as ^ { withShader* `Shader' } -> `CoglHandle' #}

--{# fun unsafe shader_get_cogl_fragment_shader as ^ { withShader* `Shader' } -> `CoglHandle' #}

--{# fun unsafe shader_get_cogl_vertex_shader as ^ { withShader* `Shader' } -> `CoglHandle' #}

--{# fun unsafe value_set_shader_float as ^
--{# fun unsafe value_get_shader_float as ^

--{# fun unsafe value_set_shader_int as ^
--{# fun unsafe value_get_shader_int as ^

--{# fun unsafe value_set_shader_matrix as ^
--{# fun unsafe value_get_shader_matrix as ^



-- | Whether the shader is compiled and linked, ready for use in the
--   GL context.
--
-- Default value: @False@
--
-- * Since 0.8
--
shaderCompiled :: ReadAttr Shader Bool
shaderCompiled = readAttrFromBoolProperty "compiled"


-- | Whether the shader is currently used in the GL rendering
--   pipeline.
--
-- Default value: @False@
--
-- * Since 0.6
--
shaderEnabled :: Attr Shader Bool
shaderEnabled = newAttrFromBoolProperty "enabled"


-- | GLSL source code for the fragment shader part of the shader
--   program.
--
-- Default value: @Nothing@
--
-- * Since 0.6
--
shaderFragmentSource :: Attr Shader (Maybe String)
shaderFragmentSource = newAttrFromMaybeStringProperty "fragment-source"

-- | GLSL source code for the vertex shader part of the shader
--   program, if any
--
-- Default value: @Nothing@
--
-- * Since 0.6
--
shaderVertexSource :: Attr Shader (Maybe String)
shaderVertexSource = newAttrFromMaybeStringProperty "vertex-source"


