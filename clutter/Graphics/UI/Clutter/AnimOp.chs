-- -*-haskell-*-
--
--  Author : Matthew Arsenault
--
--  Created: 27 Mar 2010
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

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- Split this out to avoid cycles

module Graphics.UI.Clutter.AnimOp (
  AnimOp(..),
  toPropertyLists
 ) where

{# import Graphics.UI.Clutter.StoreValue #}

import System.Glib.Attributes
import System.Glib.GObject

-- CHECKME? allow WriteAttr?  Also, Animatable class? Not all
--attributes animatable. Especially the convenient ones I added like
--position and size
--TODO: Rename to be more general, since other stuff uses it too
--data AnimOp o = forall a b. ReadWriteAttr o a b :-> b
data AnimOp o = forall a b. (GenericValueClass b) => ReadWriteAttr o a b :-> b

infixr 0 :->

--TODO: Rename these
toPropertyLists :: (GObjectClass o) => [AnimOp o] -> ([String], [GenericValue])
toPropertyLists = foldr step ([], [])
    where step (attr :-> val) (strs, vals) = (show attr:strs, toGenericValue val:vals)




