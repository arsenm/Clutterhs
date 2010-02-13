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



-- | Creates a new vertex buffer that you can use to add attributes.
--
-- [@n_vertices@] The number of vertices that your attributes will
-- correspond to.
--
-- [@Returns@] a new CoglHandle
--
{# fun unsafe vertex_buffer_new as ^ { cIntConv `Word' } -> `VertexBuffer' newVertexBuffer* #}


-- | Retrieves the number of vertices that handle represents
--
-- [@handle@] A vertex buffer handle
--
-- [@Returns@] the number of vertices
--
{# fun unsafe vertex_buffer_get_n_vertices as ^
  { withVertexBuffer* `VertexBuffer' } -> `Word' cIntConv #}

-- buffer_add



-- | This function deletes an attribute from a buffer. You will need
-- to call 'vertexBufferSubmit' or issue a draw call to commit this
-- change to the GPU.
--
-- [@handle@] A vertex buffer handle
--
-- [@attribute_name@] The name of a previously added attribute
--
{# fun unsafe vertex_buffer_delete as ^ { withVertexBuffer* `VertexBuffer', `String' } -> `()' #}


-- | This function submits all the user added attributes to the GPU;
-- once submitted the attributes can be used for drawing.
--
-- You should aim to minimize calls to this function since it implies
-- validating your data; it potentially incurs a transport cost
-- (especially if you are using GLX indirect rendering) and
-- potentially a format conversion cost if the GPU doesn't natively
-- support any of the given attribute formats.
--
-- [@handle@] A vertex buffer handle
--
{# fun unsafe vertex_buffer_submit as ^ { withVertexBuffer* `VertexBuffer' } -> `()' #}


-- | This function disables a previosuly added attribute.
--
-- Since it can be costly to add and remove new attributes to buffers;
-- to make individual buffers more reuseable it is possible to enable
-- and disable attributes before using a buffer for drawing.
--
-- You don't need to call 'vertexBufferSubmit' after using this
-- function.
--
-- [@handle@] A vertex buffer handle
--
-- [@attribute_name@] The name of the attribute you want to disable
--
{# fun unsafe vertex_buffer_disable as ^ { withVertexBuffer* `VertexBuffer', `String' } -> `()' #}


-- | This function enables a previosuly disabled attribute.
--
-- Since it can be costly to add and remove new attributes to buffers;
-- to make individual buffers more reuseable it is possible to enable
-- and disable attributes before using a buffer for drawing.
--
-- You don't need to call 'vertexBufferSubmit' after using this
-- function
--
-- [@handle@] A vertex buffer handle
--
-- [@attribute_name@] The name of the attribute you want to enable
--
{# fun unsafe vertex_buffer_enable as ^ { withVertexBuffer* `VertexBuffer', `String' } -> `()' #}


-- | This function lets you draw geometry using all or a subset of the
-- vertices in a vertex buffer.
--
-- Any un-submitted attribute changes are automatically submitted
-- before drawing.
--
-- [@handle@] A vertex buffer handle
--
-- [@mode@] A 'VerticesMode' specifying how the vertices should be
-- interpreted.
--
-- [@first@] Specifies the index of the first vertex you want to draw
-- with
--
-- [@count@] Specifies the number of vertices you want to draw.
--
{# fun unsafe vertex_buffer_draw as ^
  { withVertexBuffer* `VertexBuffer', cFromEnum `VerticesMode', `Int', `Int' } -> `()' #}


-- | Checks whether handle is a Vertex Buffer Object
--
-- [@handle@] a CoglHandle for a vertex buffer object
--
-- [@Returns@] @True@ if the handle is a VBO, and @False@ otherwise
--
-- * Since 1.0
--
{# fun unsafe is_vertex_buffer as ^ { withVertexBuffer* `VertexBuffer' } -> `Bool' #}




-- | Depending on how much geometry you are submitting it can be
-- worthwhile optimizing the number of redundant vertices you
-- submit. Using an index array allows you to reference vertices
-- multiple times, for example during triangle strips.
--
-- [@indices_array@] Specifies your list of indices
--
-- [@Returns@] A CoglHandle for the indices which you can pass to
-- 'vertexBufferDrawElements'
--
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


-- | This function lets you use an array of indices to specify the
-- vertices within your vertex buffer that you want to draw. The
-- indices themselves are created by calling 'vertexBufferIndicesNew'
--
-- Any un-submitted attribute changes are automatically submitted
-- before drawing.
--
-- [@handle@] A vertex buffer handle
--
-- [@mode@] A 'VerticesMode' specifying how the vertices should be
-- interpreted.
--
-- [@indices@] A CoglHandle for a set of indices allocated via
-- 'vertexBufferIndicesNew'
--
-- [@min_index@] Specifies the minimum vertex index contained in indices
--
-- [@max_index@]
--
-- Specifies the maximum vertex index contained in indices
--
-- [@indices_offset@] An offset into named indices. The offset marks
-- the first index to use for drawing.
--
-- [@count@] Specifies the number of vertices you want to draw.
--
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




