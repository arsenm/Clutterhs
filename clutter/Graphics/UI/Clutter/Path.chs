-- -*-haskell-*-
--  Clutter Path
--
--  Author : Matthew Arsenault
--
--  Created: 2 Oct 2009
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

-- | Path — An object describing a path with straight lines and Bezier curves.
module Graphics.UI.Clutter.Path (
-- * Detail
-- | A 'Path' contains a description of a path
-- consisting of straight lines and bezier curves. This can be used in
-- a 'BehaviourPath' to animate an actor moving along the path.
--
-- The path consists of a series of nodes. Each node is one of the following four types:
--
-- 'PathMoveTo'
--
-- Changes the position of the path to the given pair of
-- coordinates. This is usually used as the first node of a path to
-- mark the start position. If it is used in the middle of a path then
-- the path will be disjoint and the actor will appear to jump to the
-- new position when animated.
--
-- 'PathLineTo'
--
-- Creates a straight line from the previous point to the given point.
--
-- 'PathCurveTo'
--
-- Creates a bezier curve. The end of the last node is used as the
-- first control point and the three subsequent coordinates given in
-- the node as used as the other three.
--
-- 'PathClose'
--
-- Creates a straight line from the last node to the last
-- pathMoveTo node. This can be used to close a path so that
-- it will appear as a loop when animated.
--
-- The first three types have the corresponding relative versions
-- 'PathRelMoveTo', 'PathRelLineTo' and
-- 'PathRelCurveTo'. These are exactly the same except the
-- coordinates are given relative to the previous node instead of as
-- direct screen positions.
--
-- You can build a path using the node adding functions such as
-- 'pathAddLineTo'. Alternatively the path can be described
-- in a string using a subset of the SVG path syntax. See
-- 'pathAddString' for details.
--
-- * Path is available since Clutter 1.0

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Path'
-- @

-- * Types
  Path,
  PathClass,
  PathNode,
  PathCallback,

-- * Constructors
  pathNew,
  pathNewWithDescription,

-- * Methods
  pathAddMoveTo,
  pathAddRelMoveTo,
  pathAddLineTo,
  pathAddRelLineTo,
  pathAddCurveTo,
  pathAddRelCurveTo,
  pathAddClose,
  pathAddString,
  pathAddNode,
  pathAddCairoPath,
  pathGetNNodes,
  pathGetNode,
  pathGetNodes,
  pathForeach,
  pathInsertNode,
  pathRemoveNode,
  pathReplaceNode,
  pathGetDescription,
  pathSetDescription,

  pathToCairoPath,
  pathClear,
  pathGetPosition,
  pathGetLength,
-- * Attributes
  pathDescription,
  pathLength
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes
import System.Glib.Properties
import Control.Monad (liftM2)
import Graphics.Rendering.Cairo.Types (Cairo)
import qualified Graphics.Rendering.Cairo.Types as Cairo

-- | Creates a new 'Path' instance with no nodes.
{# fun unsafe path_new as ^ { } -> `Path' newPath* #}

-- | Creates a new 'Path' instance with the nodes described in
--   desc. See 'pathAddString' for details of the format of
--   the string.
--
--   [@description@] A string describing the path.
--
--   [@Returns@] The newly created 'Path'.
--
-- * Since 1.0
{# fun unsafe path_new_with_description as ^ { `String' } -> `Path' newPath* #}

-- | Adds a 'PathMoveTo' type node to the path. This is usually used as
--   the first node in a path. It can also be used in the middle of
--   the path to cause the actor to jump to the new coordinate.
--
-- [@path@]  a 'Path'
--
-- [@x@]  the x coordinate
--
-- [@y@]  the y coordinate
--
-- * Since 1.0
{# fun unsafe path_add_move_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}

-- | Same as 'pathAddMoveTo' except the coordinates are relative to the previous node.
--
-- [@path@] a 'Path'
--
-- [@x@] the x coordinate
--
-- [@y@] the y coordinate
--
-- * Since 1.0
{# fun unsafe path_add_rel_move_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}

-- | Adds a PathLineTo type node to the path. This causes the actor to
--   move to the new coordinates in a straight line.
--
-- [@path@] a Path
--
-- [@x@] the x coordinate
--
-- [@y@] the y coordinate
--
-- * Since 1.0
{# fun unsafe path_add_line_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}


-- | Same as 'pathAddLineTo' except the coordinates are relative to the previous node.
--
-- [@path@] a 'Path'
--
-- [@x@] the x coordinate
--
-- [@y@] the y coordinate
--
-- * Since 1.0
{# fun unsafe path_add_rel_line_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}

-- | Adds a PathCurveTo type node to the path. This causes
--   the actor to follow a Bezier from the last node to (x_3, y_3)
--   using (x_1, y_1) and (x_2,y_2) as control points.
--
-- [@path@] a 'Path'
--
-- [@x_1@] the x coordinate of the first control point
--
-- [@y_1@] the y coordinate of the first control point
--
-- [@x_2@] the x coordinate of the second control point
--
-- [@y_2@] the y coordinate of the second control point
--
-- [@x_3@] the x coordinate of the third control point
--
-- [@y_3@] the y coordinate of the third control point
--
-- * Since 1.0
{# fun unsafe path_add_curve_to as ^ { withPath* `Path', `Int', `Int', `Int', `Int', `Int', `Int' } -> `()' #}


-- | Same as 'pathAddCurveTo' except the coordinates are
--   relative to the previous node.
--
--  [@path@]: a 'Path'
--
--  [@x_1@] the x coordinate of the first control point
--
--  [@y_1@] the y coordinate of the first control point
--
--  [@x_2@] the x coordinate of the second control point
--
--  [@y_2@] the y coordinate of the second control point
--
--  [@x_3@] the x coordinate of the third control point
--
--  [@y_3@] the y coordinate of the third control point
--
--  * Since: 1.0
{# fun unsafe path_add_rel_curve_to as ^ { withPath* `Path', `Int', `Int', `Int', `Int', `Int', `Int' } -> `()' #}


-- |  Adds a 'PathClose' type node to the path. This creates a
--  straight line from the last node to the last 'PathMoveTo'
--  type node.
--
--  [@path@] a 'Path'
--
-- * Since: 1.0
--
{# fun unsafe path_add_close as ^ { withPath* `Path' } -> `()' #}

-- | Adds new nodes to the end of the path as described by a @String@. The
--   format is a subset of the SVG path format. Each node is represented
--   by a letter and is followed by zero, one or three pairs of
--   coordinates. The coordinates can be separated by spaces or a
--   comma. The types are:
--
--  [@M@] Adds a 'PathMoveTo' node. Takes one pair of coordinates.
--
--  [@L@] Adds a 'PathLineTo' node. Takes one pair of coordinates.
--
--  [@C@] Adds a 'PathCurveTo' node. Takes three pairs of coordinates.
--
--  [@z@] Adds a 'PathClose' node. No coordinates are needed.
--
-- The M, L and C commands can also be specified in lower case which
-- means the coordinates are relative to the previous node.
--
-- For example, to move an actor in a 100 by 100 pixel square centered
-- on the point 300,300 you could use the following path:
--
--  @ M 250,350 l 0 -100 L 350,250 l 0 100 z @
--
-- If the path description isn't valid @False@ will be returned and no nodes will be added.
--
-- [@path@] a 'Path'
--
-- [@str@] a string describing the new nodes
--
-- [@Returns@] @True@ is the path description was valid or @False@ otherwise.
--
-- * Since 1.0
--
{# fun unsafe path_add_string as ^ { withPath* `Path', `String' } -> `Bool' #}



-- | Adds a node to the end of the path.
--
--  [@path@] a 'Path'
--
--  [@node@] a 'PathNode'
--
-- * Since 1.0
{# fun unsafe path_add_node as ^ { withPath* `Path', withPathNode* `PathNode' } -> `()' #}

-- | Add the nodes of the Cairo path to the end of path
--
--  [@path@] a Path
--
--  [@cairopath@] a 'CairoPath'
--
-- * Since 1.0
--
{# fun unsafe path_add_cairo_path as ^ { withPath* `Path', withCairoPath `Cairo.Path' } -> `()' #}

-- | Retrieves the number of nodes in the path.
--
-- [@path@] A 'Path'
--
-- [@Returns@] The number of nodes
--
-- * Since 1.0
--
{# fun unsafe path_get_n_nodes as ^ { withPath* `Path' } -> `Word' cIntConv #}

-- | Retrieves the node of the path indexed by index.
--
--  [@path@] a 'Path'
--
--  [@index@] the node number to retrieve
--
--  [@Returns@] The node at the location
--
-- * Since 1.0
--
{# fun unsafe path_get_node as ^ { withPath* `Path', `Int', alloca- `PathNode' peek* } -> `()' #}

-- | Returns a list of 'PathNodes'.
--
--  [@path@] a 'Path'
--
--  [@Returns@] a list of 'PathNode's in the path
--
-- * Since 1.0
--
{# fun unsafe path_get_nodes as ^ { withPath* `Path' } -> `[PathNode]' newPathNodes* #}

-- | Calls a function for each node of the path
--
-- [@path@] a 'Path'
--
-- [@callback@] the function to call with each node
--
-- * Since 1.0
--
pathForeach :: Path -> PathCallback -> IO ()
pathForeach path cpcb = withPath path $ \pathPtr -> do
                        funcPtr <- newPathCallback cpcb
                        --CHECKME: unsafe?
                        {# call unsafe path_foreach #} pathPtr funcPtr nullPtr
                        freeHaskellFunPtr funcPtr

-- | Inserts node into the path before the node at the given
--   offset. If index is negative it will append the node to the end
--   of the path.
--
-- [@path@] a 'Path'
--
-- [@index@] offset of where to insert the node
--
-- [@node@] the node to insert
--
-- * Since 1.0
--
{# fun unsafe path_insert_node as ^ { withPath* `Path', `Int', withPathNode* `PathNode' } -> `()' #}

-- | Removes the node at the given offset from the path.
--
-- [@path@] a 'Path'
--
-- [@index@] index of the node to remove
--
-- * Since 1.0
--
{# fun unsafe path_remove_node as ^ { withPath* `Path', cIntConv `Word' } -> `()' #}

-- | Replaces the node at a given offset index with node.
--
-- [@path@] a 'Path'
--
-- [@index@] index to the existing node
--
-- [@node@] the replacement node
--
-- *Since 1.0
--
{# fun unsafe path_replace_node as ^ { withPath* `Path', cIntConv `Word', withPathNode* `PathNode' } -> `()' #}


-- | Returns a string describing the path in the same format as used by 'pathAddString'
--
-- [@path@] a 'Path'
--
-- [@Returns@] a string description of the path
--
-- * Since 1.0
--
{# fun unsafe path_get_description as ^ { withPath* `Path' } -> `String' peekNFreeString* #}

-- | Replaces all of the nodes in the path with nodes described by
--  str. See 'pathAddString' for details of the format.
--  If the string is invalid then @False@ is returned and the path is unaltered.
--
-- [@path@] a 'Path'
--
-- [@str@] a string describing the path
--
-- [@Returns@] @True@ if the path was valid, @False@ otherwise
--
-- * Since 1.0
--
{# fun unsafe path_set_description as ^ { withPath* `Path', `String' } -> `Bool' #}

-- | Add the nodes of the ClutterPath to the path in the Cairo context.
--
-- [@path@] a 'Path'
--
-- [@cairo@] a Cairo context
--
-- * Since 1.0
--
{# fun unsafe path_to_cairo_path as ^ { withPath* `Path', withCairo `Cairo' } -> `()' #}

-- | Removes all nodes from the path.
--
-- [@path@] a 'Path'
--
-- * Since 1.0
--
{# fun unsafe path_clear as ^ { withPath* `Path' } -> `()' #}

--CHECKME: Do you want to get the position out? or is it just storage?
--CHECKME: Maybe not use type Knot = (Int, Int)
--CHECKME: is this what I want to return?

-- | The value in progress represents a position along the path where
--   0.0 is the beginning and 1.0 is the end of the path. An
--   interpolated position is then stored in position.
--
-- [@path@] a 'Path'
--
-- [@progress@] a position along the path as a fraction of its length
--
-- [@position@] location to store the position
--
-- [@Returns@] index of the node used to calculate the position.
--
-- * Since 1.0
--
pathGetPosition :: Path -> Double -> IO (Word, Knot)
pathGetPosition path progress = withPath path $ \pathptr ->
                                alloca $ \kptr -> do
                                   ret <- ({# call unsafe path_get_position #} pathptr (cFloatConv progress) (castPtr kptr))
                                   val <- peek kptr
                                   return (cIntConv ret, val)

-- | Retrieves an approximation of the total length of the path.
--
-- [@path@] a 'Path'
--
-- [@Returns@] the length of the path
--
-- * Since 1.0
--
{# fun unsafe path_get_length as ^ { withPath* `Path' } -> `Word' cIntConv #}


-- | SVG-style description of the path.
--
-- Default value: \"\"
--
pathDescription :: Attr Path String
pathDescription = newAttrFromStringProperty "description"

-- | An approximation of the total length of the path.
--
-- Default value: 0
--
pathLength :: ReadAttr Path Word
pathLength = readNamedAttr "length" pathGetLength

