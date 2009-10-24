-- -*-haskell-*-
--  Clutter Interval
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

module Graphics.UI.Clutter.Interval (
                                     intervalNew
                                     intervalNewWithValues,
                                   --intervalClone,
                                   --intervalGetValueType,
                                   --intervalSetInitialValue,
                                   --intervalGetInitialValue,
                                   --intervalInitialValue,
                                   --intervalPeekInitalValue,
                                   --intervalSetFinalValue,
                                   --intervalGetFinalValue,
                                   --intervalFinalValue,
                                   --intervalPeekFinalValue,
                                   --intervalSetInterval,
                                   --intervalGetInterval,
                                   --intervalInterval,
                                   --intervalComputeValue,
                                   --intervalValidate,
                                   --intervalRegisterProgressFunc
                                    ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import System.Glib.GObject
import qualified System.Glib.GTypeConstants as GType

intervalNew = error "ClutterInterval unimplemented"

--this seems to replace intervalNew for language bindings so rename it?
--FIXME: toGValue and then pattern match against constructors is dumb
--but I'm too lazy to fix it now
intervalNewWithValues :: (GValueArgClass arg) => arg -> arg -> IO Interval
intervalNewWithValues initial final = let func = {# call unsafe interval_new_with_values #}
                                          typedFunc = case toGValueArg initial of
                                                        (UInteger _) -> func GType.int
                                                        (UDouble _) ->  func GType.double
                                                        (UFloat _) -> func GType.float
                                                        (UString _) ->  func GType.string
                                                        (UChar _) ->  func GType.char
                                                        (UUChar _) -> func GType.uchar
                                                        (UColor _) -> func color
                                                        (UGObject _) -> func GType.object
                                      in withGValueArg initial $ \argptr1 ->
                                          withGValueArg final $ \argptr2 ->
                                            newInterval =<< typedFunc argptr1 argptr2


