-- -*-haskell-*-
--  Clutter Animatable
--
--  Author : Matthew Arsenault
--
--  Created: 25 Oct 2009
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

module Graphics.UI.Clutter.Animatable (
                                       animatableAnimateProperty
                                      ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import System.Glib.GObject
{-
animatableAnimateProperty :: (AnimatableClass animatable, GValueArgClass val) =>
                             animatable ->
                             Animation ->
                             String ->
                             val ->
                             val ->
                             Double ->
                             val ->
                             IO (Maybe val)
-}
animatableAnimateProperty = undefined
{-
TODO: Requires getting out the value from the gvalue for this to be meaningful
How to deal with gvalues still not good
animatableAnimateProperty animAble anim pName initial final prog =
--CHECKME: unsafe?
    let func = {# call unsafe animatable_animate_property #}
    in withAnimatableClass animAble $ \animAblePtr ->
         withAnimation anim $ \animPtr ->
           withCString pName $ \strPtr ->
             withGValueArg initial $ \initPtr ->
               withGValueArg final $ \finPtr ->
                 allocaGValue $ \valPtr -> do
                   res <- func animAblePtr animPtr strPtr initPtr finPtr (cFloatConv prog) valPtr
                   return $ if res
                              then Just valPtr
                              else Prelude.Nothing
-}

