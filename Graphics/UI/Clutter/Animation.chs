-- -*-haskell-*-
--  Clutter Animation
--
--  Author : Matthew Arsenault
--
--  Created: 20 Sep 2009
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

module Graphics.UI.Clutter.Animation (
                                      animationNew,
                                    --animationSetObject,
                                    --animationGetObject,
                                    --animationObject,

                                      animationSetDuration,
                                      animationGetDuration,
                                      animationDuration,

                                      animationSetLoop,
                                      animationGetLoop,
                                      animationLoop,

                                    --animationSetTimeline,
                                    --animationGetTimeline,
                                    --animationTimeline,

                                    --animationSetAlpha,
                                    --animationGetAlpha,
                                    --animationAlpha,

                                      animationCompleted,
                                    --animationBind,
                                    --animationBindInterval,
                                    --animationUpdateInterval,
                                    --animationHasProperty,
                                    --animationUnbindProperty,
                                    --animationGetInterval,

                                    --actorAnimate,
                                    --actorAnimateWithTimeline,
                                    --actorAnimateWithAlpha,
                                    --actorAnimatev,
                                    --actorAnimatevwithTimelinev,
                                    --actorAnimatevWithAlphav,
                                    --actorGetAnimation
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties

{# fun unsafe animation_new as ^ {} -> `Animation' newAnimation* #}

{# fun unsafe animation_set_duration as ^ { withAnimation* `Animation', `Int' } -> `()' #}
--FIXME: Set an gint, get a guint? what? why?
{# fun unsafe animation_get_duration as ^ { withAnimation* `Animation' } -> `Int' #}
animationDuration :: Attr Animation Int
animationDuration = newAttr animationGetDuration animationSetDuration


{# fun unsafe animation_set_loop as ^ { withAnimation* `Animation', `Bool' } -> `()' #}
{# fun unsafe animation_get_loop as ^ { withAnimation* `Animation' } -> `Bool' #}
animationLoop :: Attr Animation Bool
animationLoop = newAttr animationGetLoop animationSetLoop

{# fun animation_completed as ^ { withAnimation* `Animation' } -> `()' #}

