-- -*-haskell-*-
--  Clutter ListModel
--
--  Author : Matthew Arsenault
--
--  Created: 31 Oct 2009
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

module Graphics.UI.Clutter.ListModel (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Model'
-- |         +----'ListModel'
-- @

-- * Constructors
  listModelNew
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GType

listModelNew :: [(GType, String)] -> IO ListModel
listModelNew lst = let (types, names) = unzip lst
                   in withMany withCString names $ \cstrs ->
                       withArrayLen cstrs $ \len namesPtr ->
                       withArray types $ \typesPtr ->
                          newListModel =<< {# call unsafe list_model_newv #} (cIntConv len) typesPtr namesPtr


