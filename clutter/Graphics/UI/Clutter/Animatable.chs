-- -*-haskell-*-
--  Clutter Animatable
--
--  Author : Matthew Arsenault
--
--  Created: 25 Oct 2009
--
--  Copyright (C) 2009-2010 Matthew Arsenault
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

module Graphics.UI.Clutter.Animatable (
-- * Class Hierarchy
-- |
-- @
-- |  'GInterface'
-- |   +----'Animatable'
-- @

-- * Types
  AnimatableClass,

-- * Methods
  animatableAnimateProperty
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.StoreValue #}

import C2HS
import System.Glib.GValue

-- I don' think this is useful....
--TODO: Get rid of string usage. Also not sure about constraint on animatable
animatableAnimateProperty :: (AnimatableClass animatable, GenericValueClass val) =>
                             animatable ->
                             Animation animatable ->
                             String ->
                             val ->
                             val ->
                             Double ->
                             IO (Maybe val)
animatableAnimateProperty animAble anim pName initial final prog =
    let func = {# call animatable_animate_property #}
    in withAnimatableClass animAble $ \animAblePtr ->
         withAnimation anim $ \animPtr ->
           withCString pName $ \strPtr ->
             withGenericValue initial $ \initPtr ->
               withGenericValue final $ \finPtr ->
                 allocaGValue $ \gv@(GValue valPtr) -> do
                   res <- func animAblePtr animPtr strPtr initPtr finPtr (cFloatConv prog) (castPtr valPtr)
                   if cToBool res
                     then (Just . unsafeExtractGenericValue) `fmap` valueGetGenericValue gv
                     else return Prelude.Nothing


