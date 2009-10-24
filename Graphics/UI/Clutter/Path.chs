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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Path (
                                 pathNew,
                                 pathNewWithDescription,
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
                                 pathInsertNode,
                                 pathRemoveNode,
                                 pathReplaceNode,
                                 pathGetDescription,
                                 pathSetDescription,
                               --pathDescription,
                                 pathToCairoPath,
                                 pathClear,
                                 pathGetPosition,
                                 pathGetLength
                               --pathCopy,
                               --pathFree,
                               --pathEqual
                                ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM, liftM2)
import System.Glib.Attributes
import Graphics.Rendering.Cairo.Types (Cairo, unCairo)
import qualified Graphics.Rendering.Cairo.Types as Cairo

{# fun unsafe path_new as ^ { } -> `Path' newPath* #}
{# fun unsafe path_new_with_description as ^ { `String' } -> `Path' newPath* #}

{# fun unsafe path_add_move_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}
{# fun unsafe path_add_rel_move_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}
{# fun unsafe path_add_line_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}
{# fun unsafe path_add_rel_line_to as ^ { withPath* `Path', `Int', `Int' } -> `()' #}
{# fun unsafe path_add_curve_to as ^ { withPath* `Path', `Int', `Int', `Int', `Int', `Int', `Int' } -> `()' #}
{# fun unsafe path_add_rel_curve_to as ^ { withPath* `Path', `Int', `Int', `Int', `Int', `Int', `Int' } -> `()' #}
{# fun unsafe path_add_close as ^ { withPath* `Path' } -> `()' #}
{# fun unsafe path_add_string as ^ { withPath* `Path', `String' } -> `Bool' #}

{# fun unsafe path_add_node as ^ { withPath* `Path', withPathNode* `PathNode' } -> `()' #}

{# fun unsafe path_add_cairo_path as ^ { withPath* `Path', withCairoPath `Cairo.Path' } -> `()' #}

--FIXME: GUInt
{# fun unsafe path_get_n_nodes as ^ { withPath* `Path' } -> `Int' #}

{# fun unsafe path_get_node as ^ { withPath* `Path', `Int', alloca- `PathNode' peek* } -> `()' #}
{# fun unsafe path_get_nodes as ^ { withPath* `Path' } -> `[PathNode]' newPathNodes* #}

pathForeach path cpcb = withPath path $ \pathPtr -> do
                        funcPtr <- newPathCallback cpcb
                        --CHECKME: unsafe?
                        {# call unsafe path_foreach #} pathPtr funcPtr nullPtr
                        freeHaskellFunPtr funcPtr

--FIXME: More guint
{# fun unsafe path_insert_node as ^ { withPath* `Path', `Int', withPathNode* `PathNode' } -> `()' #}
{# fun unsafe path_remove_node as ^ { withPath* `Path', `Int' } -> `()' #}
{# fun unsafe path_replace_node as ^ { withPath* `Path', `Int', withPathNode* `PathNode' } -> `()' #}

--FIXME: Attribute with the returning of bool from set description for success I think won't work
{# fun unsafe path_get_description as ^ { withPath* `Path' } -> `String' #}
{# fun unsafe path_set_description as ^ { withPath* `Path', `String' } -> `Bool' #}
--pathDescription :: Attr Path String
--pathDescription = newAttr pathGetDescription pathSetDescription

{# fun unsafe path_to_cairo_path as ^ { withPath* `Path', withCairo `Cairo' } -> `()' #}

{# fun unsafe path_clear as ^ { withPath* `Path' } -> `()' #}

--CHECKME: Do you want to get the position out? or is it just storage?
--CHECKME: Maybe not use type Knot = (Int, Int)
--CHECKME: is this what I want to return?
pathGetPosition :: Path -> Double -> IO (GUInt, Knot)
pathGetPosition path progress = withPath path $ \pathptr ->
                                alloca $ \kptr ->
                                   liftM2 (,) ({# call unsafe path_get_position #} pathptr (cFloatConv progress) (castPtr kptr))
                                             (peek kptr)

{# fun unsafe path_get_length as ^ { withPath* `Path' } -> `GUInt' cIntConv #}

--these unnecessary
--{# fun unsafe path_node_copy as ^ { withPathNode* `PathNode' } -> `PathNode' #}
--{# fun unsafe path_node_free as ^ { withPathNode* `PathNode' } -> `()' #}
--{# fun unsafe path_node_equal as ^ { withPathNode* `PathNode', withPathNode* `PathNode' } -> `()' #}


