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
                                     intervalNew,
                                     intervalNewWithValues,
                                     intervalClone,
                                     intervalGetValueType,
                                     intervalSetInitialValue,
                                   --intervalGetInitialValue,
                                   --intervalInitialValue,
                                   --intervalPeekInitalValue,
                                     intervalSetFinalValue,
                                   --intervalGetFinalValue,
                                   --intervalFinalValue,
                                   --intervalPeekFinalValue,
                                   --intervalSetInterval,
                                   --intervalGetInterval,
                                   --intervalInterval,
                                     intervalComputeValue,
                                   --intervalValidate,
                                   --intervalRegisterProgressFunc
                                    ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import System.Glib.GObject
import System.Glib.GType
import qualified System.Glib.GTypeConstants as GType

intervalNew = error "ClutterInterval unimplemented"

--this seems to replace intervalNew for language bindings so rename
--it?  FIXME: toGValue and then pattern match against constructors is
--kind of dumb but I'm too lazy to fix it now
gValueArgGType val = case toGValueArg val of
                       (UInteger _) -> GType.int
                       (UDouble _) ->  GType.double
                       (UFloat _) -> GType.float
                       (UString _) -> GType.string
                       (UChar _) ->  GType.char
                       (UUChar _) -> GType.uchar
                       (UColor _) -> color
                       (UGObject _) -> GType.object


intervalNewWithValues :: (GValueArgClass arg) => arg -> arg -> IO Interval
intervalNewWithValues initial final = let func = {# call unsafe interval_new_with_values #}
                                          gtype = gValueArgGType initial
                                      in withGValueArg initial $ \argptr1 ->
                                          withGValueArg final $ \argptr2 ->
                                            newInterval =<< func gtype argptr1 argptr2

{# fun unsafe interval_clone as ^
       { withInterval* `Interval' } -> `Interval' newInterval* #}


{# fun unsafe interval_set_initial_value as ^
   `(GValueArgClass value)' => { withInterval* `Interval', withGValueArg* `value' } -> `()' #}

{# fun unsafe interval_set_final_value as ^
   `(GValueArgClass value)' => { withInterval* `Interval', withGValueArg* `value' } -> `()' #}

--TODO: How to get out GValue?
--Clutter also has set way using varargs to avoid gvalues,
--but not a arg vector version

{# fun unsafe interval_compute_value as ^
       `(GValueArgClass value)' =>
       { withInterval* `Interval',
         `Double',
         withGValueArg* `value' } -> `Bool' #}

{# fun unsafe interval_get_value_type as ^
       { withInterval* `Interval' } -> `GType' cToEnum #}


--ProgressFunc: grar gvalues cause me pain


