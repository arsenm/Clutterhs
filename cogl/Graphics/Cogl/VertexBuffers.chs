-- -*-haskell-*-
--  COGL Vertex Buffers
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

-- | Vertex Buffers — An API for submitting extensible arrays of
-- vertex attributes to be mapped into the GPU for fast drawing.
module Graphics.Cogl.VertexBuffers (
  VertexBuffer,
  VertexIndices,
  IndicesType(..),

  vertexBufferNew,
  vertexBufferGetNVertices,
--vertexBufferAdd,
  vertexBufferDelete,
  vertexBufferSubmit,
  vertexBufferDisable,
  vertexBufferEnable,
  vertexBufferDraw,
  isVertexBuffer,

  vertexBufferDrawElements,
  vertexBufferIndicesGetForQuads
) where

import C2HS

{# import Graphics.Cogl.Types #}
{# import Graphics.Cogl.Enums #}


{# fun unsafe vertex_buffer_new as ^ { cIntConv `Word' } -> `VertexBuffer' newVertexBuffer* #}

{# fun unsafe vertex_buffer_get_n_vertices as ^
  { withVertexBuffer* `VertexBuffer' } -> `Word' cIntConv #}

-- buffer_add

{# fun unsafe vertex_buffer_delete as ^ { withVertexBuffer* `VertexBuffer', `String' } -> `()' #}

{# fun unsafe vertex_buffer_submit as ^ { withVertexBuffer* `VertexBuffer' } -> `()' #}

{# fun unsafe vertex_buffer_disable as ^ { withVertexBuffer* `VertexBuffer', `String' } -> `()' #}

{# fun unsafe vertex_buffer_enable as ^ { withVertexBuffer* `VertexBuffer', `String' } -> `()' #}


{# fun unsafe vertex_buffer_draw as ^
  { withVertexBuffer* `VertexBuffer', cFromEnum `VerticesMode', `Int', `Int' } -> `()' #}

{# fun unsafe is_vertex_buffer as ^ { withVertexBuffer* `VertexBuffer' } -> `Bool' #}

class IndicesType a where
  vertexBufferIndicesNew :: [a] -> IO (VertexIndices a)

instance IndicesType Word8 where
  vertexBufferIndicesNew indices = let func = {# call unsafe vertex_buffer_indices_new #}
                                   in withArrayLen indices $ \len iPtr -> do
                                        h <- func (cFromEnum IndicesTypeUnsignedByte)
                                                   (castPtr iPtr)
                                                   (cIntConv len)
                                        vi <- newVertexIndicesRaw h
                                        return $ mkVertexIndices (undefined :: Word8) vi

instance IndicesType Word16 where
  vertexBufferIndicesNew indices = let func = {# call unsafe vertex_buffer_indices_new #}
                                   in withArrayLen indices $ \len iPtr -> do
                                        h <- func (cFromEnum IndicesTypeUnsignedShort)
                                                   (castPtr iPtr)
                                                   (cIntConv len)
                                        vi <- newVertexIndicesRaw h
                                        return $ mkVertexIndices (undefined :: Word16) vi


{# fun unsafe vertex_buffer_draw_elements as ^
  { withVertexBuffer* `VertexBuffer',
    cFromEnum `VerticesMode',
    withVertexIndices* `VertexIndices a',
    `Int',
    `Int',
    `Int',
    `Int' } -> `()' #}


--FIXME: This should not be unreffed
{# fun unsafe vertex_buffer_indices_get_for_quads as ^
  { cIntConv `Word' } -> `VertexBuffer' newVertexBuffer* #}




