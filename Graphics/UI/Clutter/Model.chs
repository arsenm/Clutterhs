-- -*-haskell-*-
--  Clutter Model
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

module Graphics.UI.Clutter.Model (
                                  modelSetNames,
                                  modelSetTypes,

                                  listModelNew
                                 ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.GType
import qualified System.Glib.GTypeConstants as GType

modelSetNames = undefined

modelSetTypes = undefined

listModelNew :: [(GType, String)] -> IO ListModel
listModelNew lst = let (types, names) = unzip lst
                   in withMany withCString names $ \cstrs ->
                       withArrayLen cstrs $ \len namesPtr ->
                       withArray types $ \typesPtr ->
                          newListModel =<< {# call unsafe list_model_newv #} (cIntConv len) typesPtr namesPtr

