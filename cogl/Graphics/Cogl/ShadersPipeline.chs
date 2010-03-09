-- -*-haskell-*-
--  COGL Shaders and Programmable Pipeline
--
--  Author : Matthew Arsenault
--
--  Created: 7 Jan 2010
--
--  Copyright (C) 2010 Matthew Arsenault
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

#include <cogl/cogl.h>

{# context lib="cogl" prefix="cogl" #}

-- | Shaders and Programmable Pipeline — Fuctions for accessing the
-- programmable GL pipeline
module Graphics.Cogl.ShadersPipeline (
  createVertexShader,
  createFragmentShader,
  shaderSource,
  shaderCompile,
  shaderGetInfoLog,
--shaderGetType,
  shaderIsCompiled,

  createProgram,
  programAttachShader,
  programLink,
  programUse,
  programGetUniformLocation,
  programUniform1f,
  programUniform1i,
--programUniformFloat,
--programUniformInt,
--programUniformMatrix
) where

import C2HS

{# import Graphics.Cogl.Types #}
{# import Graphics.Cogl.Enums #}

createVertexShader :: IO VertexShader
createVertexShader = newVertexShader =<< {# call unsafe create_shader #} (cFromEnum ShaderTypeVertex)

createFragmentShader :: IO FragmentShader
createFragmentShader = newFragmentShader =<< {# call unsafe create_shader #} (cFromEnum ShaderTypeFragment)

{# fun unsafe shader_source as ^ `(ShaderClass shader)' => { withShader* `shader', `String' } -> `()' #}

{# fun unsafe shader_compile as ^ `(ShaderClass shader)' => { withShader* `shader' } -> `()' #}

{# fun unsafe shader_get_info_log as ^ `(ShaderClass shader)' => { withShader* `shader' } -> `String' peekNFreeString* #}

--{# fun unsafe shader_get_type as ^ `(ShaderClass shader)' => { withShader* `shader' } -> `ShaderType' cToEnum #}

{# fun unsafe shader_is_compiled as ^ `(ShaderClass shader)' => { withShader* `shader' } -> `Bool' #}


peekNFreeString :: Ptr CChar -> IO String
peekNFreeString p = do
                ret <- peekCString p
                free p
                return ret


{# fun unsafe create_program as ^ { } -> `Program' newProgram* #}

{# fun unsafe program_attach_shader as ^ `(ShaderClass shader)' =>
  { withProgram* `Program', withShader* `shader' } -> `()' #}

{# fun unsafe program_link as ^ { withProgram* `Program' } -> `()' #}

{# fun unsafe program_use as ^ { withProgram* `Program' } -> `()' #}

{# fun unsafe program_get_uniform_location as ^ { withProgram* `Program', `String' } -> `Int' #}

{# fun unsafe program_uniform_1f as ^ { `Int', `Float' } -> `()' #}

{# fun unsafe program_uniform_1i as ^ { `Int', `Int' } -> `()' #}

-- program_uniform_float
-- program_uniform_int
-- program_uniform_matrix