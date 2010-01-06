-- -*-haskell-*-
--  COGL Primitives
--
--  Author : Matthew Arsenault
--
--  Created: 18 Dec 2009
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

#include <cogl/cogl.h>

{# context lib="cogl" prefix="cogl" #}

-- | Primitives — Functions that draw various primitive shapes and
-- allow for construction of more complex paths.
module Graphics.Cogl.Primitives (
  rectangle,
  rectangles,
  rectangleWithTextureCoords,
--rectanglesWithTextureCoords,
  rectangleWithMultitextureCoords,
--polygon,

  pathNew,
  pathMoveTo,
  pathClose,
  pathLineTo,
  pathCurveTo,
  pathArc,
  pathRelMoveTo,
  pathRelLineTo,
  pathRelCurveTo,
  pathLine,
  pathPolyline,
  pathPolygon,
  pathRectangle,
  pathRoundRectangle,
  pathEllipse,
  pathFill,
  pathFillPreserve,
  pathStroke,
  pathStrokePreserve,
--  color
) where

import C2HS
import Foreign.Storable.Tuple
import Control.Monad (zipWithM_)

{# import Graphics.Cogl.Types #}


-- | Fills a rectangle at the given coordinates with the current
-- source material
--
-- [@x_1@] X coordinate of the top-left corner
--
-- [@y_1@] Y coordinate of the top-left corner
--
-- [@x_2@] X coordinate of the bottom-right corner
--
-- [@y_2@] Y coordinate of the bottom-right corner
--
{# fun unsafe rectangle as ^
  { `Float', `Float', `Float', `Float' } -> `()' #}


-- | Draws a series of rectangles in the same way that 'rectangle'
-- does. In some situations it can give a significant performance
-- boost to use this function rather than calling 'rectangle'
-- separately for each rectangle.
--
-- Each group of 4 values corresponds to the parameters x1, y1, x2,
-- and y2, and have the same meaning as in 'rectangle'.
--
-- [@verts@] a list of vertices.
--
-- * Since 1.0
--
rectangles :: [(Float, Float, Float, Float)] -> IO ()
rectangles pts = withArrayLen pts $ \len ptr ->
  {# call unsafe rectangles #} (castPtr ptr) (cIntConv len)


-- | Draw a rectangle using the current material and supply texture
-- coordinates to be used for the first texture layer of the
-- material. To draw the entire texture pass in tx1=0.0 ty1=0.0
-- tx2=1.0 ty2=1.0.
--
-- [@x1@] x coordinate upper left on screen.
--
-- [@y1@] y coordinate upper left on screen.
--
-- [@x2@] x coordinate lower right on screen.
--
-- [@y2@] y coordinate lower right on screen.
--
-- [@tx1@] x part of texture coordinate to use for upper left pixel
--
-- [@ty1@] y part of texture coordinate to use for upper left pixel
--
-- [@tx2@] x part of texture coordinate to use for lower right pixel
--
-- [@ty2@] y part of texture coordinate to use for left pixel
--
-- * Since 1.0
--
{# fun unsafe rectangle_with_texture_coords as ^
  { `Float', `Float', `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}

--TODO: This function
-- {# fun unsafe rectangles_with_texture_coords

--CHECKME: Array size
-- | This function draws a rectangle using the current source material
-- to texture or fill with. As a material may contain multiple texture
-- layers this interface lets you supply texture coordinates for each
-- layer of the material.
--
-- The first pair of coordinates are for the first layer (with the
-- smallest layer index) and if you supply less texture coordinates
-- than there are layers in the current source material then default
-- texture coordinates (0.0, 0.0, 1.0, 1.0) are generated.
--
-- [@x1@] x coordinate upper left on screen.
--
-- [@y1@] y coordinate upper left on screen.
--
-- [@x2@] x coordinate lower right on screen.
--
-- [@y2@] y coordinate lower right on screen.
--
-- [@tex_coords@] texture coordinates
--
-- * Since 1.0
--
rectangleWithMultitextureCoords :: Float -> Float -> Float -> Float -> [(Float, Float)] -> IO ()
rectangleWithMultitextureCoords x1 y1 x2 y2 coords = let func = {# call unsafe rectangle_with_multitexture_coords #}
                                                         cx1 = cFloatConv x1
                                                         cy1 = cFloatConv y1
                                                         cx2 = cFloatConv x2
                                                         cy2 = cFloatConv y2
                                                     in withArrayLen coords $ \len ptr ->
                                                          func cx1 cy1 cx2 cy2 (castPtr ptr) (cIntConv len)

-- | Clears the current path and starts a new one.
--
-- * Since 1.0
--
{# fun unsafe path_new as ^ { } -> `()' #}

-- | Moves the pen to the given location. If there is an existing path
-- this will start a new disjoint subpath.
--
-- [@x@] X coordinate of the pen location to move to.
--
-- [@y@] Y coordinate of the pen location to move to.
--
{# fun unsafe path_move_to as ^ { `Float' , `Float' } -> `()' #}

-- | Closes the path being constructed by adding a straight line
-- segment to it that ends at the first vertex of the path.
--
{# fun unsafe path_close as ^ { } -> `()' #}


-- | Adds a straight line segment to the current path that ends at the
-- given coordinates.
--
-- [@x@] X coordinate of the end line vertex
--
-- [@y@] Y coordinate of the end line vertex
--
{# fun unsafe path_line_to as ^ { `Float' , `Float' } -> `()' #}


-- | Adds a cubic bezier curve segment to the current path with the
-- given second, third and fourth control points and using current pen
-- location as the first control point.
--
-- [@x_1@] X coordinate of the second bezier control point
--
-- [@y_1@] Y coordinate of the second bezier control point
--
-- [@x_2@] X coordinate of the third bezier control point
--
-- [@y_2@] Y coordinate of the third bezier control point
--
-- [@x_3@] X coordinate of the fourth bezier control point
--
-- [@y_3@] Y coordinate of the fourth bezier control point
--
{# fun unsafe path_curve_to as ^
  { `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}


-- | Adds an elliptical arc segment to the current path. A straight
-- line segment will link the current pen location with the first
-- vertex of the arc. If you perform a move_to to the arcs start just
-- before drawing it you create a free standing arc.
--
-- [@center_x@] X coordinate of the elliptical arc center
--
-- [@center_y@] Y coordinate of the elliptical arc center
--
-- [@radius_x@] X radius of the elliptical arc
--
-- [@radius_y@] Y radius of the elliptical arc
--
-- [@angle_1@] Angle in the unit-circle at which the arc begins
--
-- [@angle_2@] Angle in the unit-circle at which the arc ends
--
{# fun unsafe path_arc as ^
  { `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}


-- | Moves the pen to the given offset relative to the current pen
-- location. If there is an existing path this will start a new
-- disjoint subpath.
--
-- [@x@] X offset from the current pen location to move the pen to.
--
-- [@y@] Y offset from the current pen location to move the pen to.
--
{# fun unsafe path_rel_move_to as ^ { `Float' , `Float' } -> `()' #}


-- | Adds a straight line segment to the current path that ends at the
-- given coordinates relative to the current pen location.
--
-- [@x@] X offset from the current pen location of the end line vertex
--
-- [@y@] Y offset from the current pen location of the end line vertex
--
{# fun unsafe path_rel_line_to as ^ { `Float' , `Float' } -> `()' #}


-- | Adds a cubic bezier curve segment to the current path with the
-- given second, third and fourth control points and using current pen
-- location as the first control point. The given coordinates are
-- relative to the current pen location.
--
-- [@x_1@] X coordinate of the second bezier control point
--
-- [@y_1@] Y coordinate of the second bezier control point
--
-- [@x_2@] X coordinate of the third bezier control point
--
-- [@y_2@] Y coordinate of the third bezier control point
--
-- [@x_3@] X coordinate of the fourth bezier control point
--
-- [@y_3@] Y coordinate of the fourth bezier control point
--
{# fun unsafe path_rel_curve_to as ^
  { `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}


-- | Constructs a straight line shape starting and ending at the given
-- coordinates. If there is an existing path this will start a new
-- disjoint sub-path.
--
-- [@x_1@] X coordinate of the start line vertex
--
-- [@y_1@] Y coordinate of the start line vertex
--
-- [@x_2@] X coordinate of the end line vertex
--
-- [@y_2@] Y coordinate of the end line vertex
--
{# fun unsafe path_line as ^ { `Float', `Float', `Float', `Float' } -> `()' #}

-- | Constructs a series of straight line segments, starting from the
-- first given vertex coordinate. If there is an existing path this
-- will start a new disjoint sub-path. Each subsequent segment starts
-- where the previous one ended and ends at the next given vertex
-- coordinate.

-- | The first value of each pair represents the X coordinate of the
-- first vertex, the second value represents the Y coordinate of the
-- first vertex, continuing in the same fashion for the rest of the
-- vertices. (num_points - 1) segments will be constructed.
--
-- [@coords@] pairs of vertices
--
pathPolyline :: [(Float, Float)] -> IO ()
pathPolyline pts = withArrayLen pts $ \len ptr ->
                     {# call unsafe path_polyline #} (castPtr ptr) (cIntConv len)


-- | Constructs a polygonal shape of the given number of vertices. If
-- there is an existing path this will start a new disjoint sub-path.
--
-- The first value of each pair represents the X coordinate of the
-- first vertex, the second value represents the Y coordinate of the
-- first vertex, continuing in the same fashion for the rest of the
-- vertices.
--
-- [@coords@] pairs of points
--
pathPolygon :: [(Float, Float)] -> IO ()
pathPolygon pts = withArrayLen pts $ \len ptr ->
                    {# call unsafe path_polygon #} (castPtr ptr) (cIntConv len)

-- | Constructs a rectangular shape at the given coordinates. If there
-- is an existing path this will start a new disjoint sub-path.
--
-- [@x_1@] X coordinate of the top-left corner.
--
-- [@y_1@] Y coordinate of the top-left corner.
--
-- [@x_2@] X coordinate of the bottom-right corner.
--
-- [@y_2@] Y coordinate of the bottom-right corner.
--
{# fun unsafe path_rectangle as ^
  { `Float', `Float', `Float', `Float' } -> `()' #}


-- | Constructs a rectangular shape with rounded corners. If there is
-- an existing path this will start a new disjoint sub-path.
--
-- [@x_1@] X coordinate of the top-left corner.
--
-- [@y_1@] Y coordinate of the top-left corner.
--
-- [@x_2@] X coordinate of the bottom-right corner.
--
-- [@y_2@] Y coordinate of the bottom-right corner.
--
-- [@radius@] Radius of the corner arcs.
--
-- [@arc_step@] Angle increment resolution for subdivision of the corner arcs.
--
{# fun unsafe path_round_rectangle as ^
  { `Float', `Float', `Float', `Float', `Float', `Float' } -> `()' #}


-- | Constructs an ellipse shape. If there is an existing path this
-- will start a new disjoint sub-path.
--
-- [@center_x@] X coordinate of the ellipse center
--
-- [@center_y@] Y coordinate of the ellipse center
--
-- [@radius_x@] X radius of the ellipse
--
-- [@radius_y@] Y radius of the ellipse
--
--
{# fun unsafe path_ellipse as ^
  { `Float', `Float', `Float', `Float' } -> `()' #}


-- | Fills the constructed shape using the current drawing color. The
-- current path is then cleared. To use the path again, call
-- 'pathFillPreserve' instead.
--
{# fun unsafe path_fill as ^ { } -> `()' #}


-- | Fills the constructed shape using the current drawing color and
-- preserves the path to be used again.
--
-- * Since 1.0
--
{# fun unsafe path_fill_preserve as ^ { } -> `()' #}


-- | Strokes the constructed shape using the current drawing color and
-- a width of 1 pixel (regardless of the current transformation
-- matrix). To current path is then cleared. To use the path again,
-- call 'pathStrokePreserve' instead.
{# fun unsafe path_stroke as ^ { } -> `()' #}


-- | Strokes the constructed shape using the current drawing color and
-- preserves the path to be used again.
--
-- * Since 1.0
--
{# fun unsafe path_stroke_preserve as ^ { } -> `()' #}


