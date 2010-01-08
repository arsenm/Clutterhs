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
  vertexBufferNew,
  vertexBufferGetNVertices,
--vertexBufferAdd,
  vertexBufferDelete,
  vertexBufferSubmit,
  vertexBufferDisable,
  vertexBufferEnable,
  vertexBufferDraw,
  isVertexBuffer,
--vertexBufferIndicesNew,
--vertexBufferDrawElements,
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

-- buffer_indices_new
--bufferIndicesNew :: IndicesType -> [graah


-- {# fun unsafe vertex_buffer_draw_elements as ^


{# fun unsafe vertex_buffer_indices_get_for_quads as ^
  { cIntConv `Word' } -> `VertexBuffer' newVertexBuffer* #}




