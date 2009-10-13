-- -*-haskell-*-
--  Clutter BehaviourPath
--
--  Author : Matthew Arsenault
--
--  Created: 13 Oct 2009
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

module Graphics.UI.Clutter.BehaviourPath (
                                          behaviourPathNew,
                                          behaviourPathNewWithDescription,
                                        --behaviourPathNewWithKnots,
                                          behaviourPathSetPath,
                                          behaviourPathGetPath,
                                          behaviourPathPath,
                                        --knotCopy, --not needed
                                        --knotFree,
                                        --knotEqual
                                         ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe behaviour_path_new as ^
       { withAlpha* `Alpha', withPath* `Path' } -> `BehaviourPath' newBehaviourPath* #}
{# fun unsafe behaviour_path_new_with_description as ^
       { withAlpha* `Alpha', `String' } -> `BehaviourPath' newBehaviourPath* #}

--TODO: Do this function
--{# fun unsafe behaviour_path_new_with_knots as ^
--       { withAlpha* `Alpha', `[Knots]', `String' } -> `BehaviourPath' newBehaviourPath* #}

{# fun unsafe behaviour_path_set_path as ^
       { withBehaviourPath* `BehaviourPath', withPath* `Path'} -> `()' #}
{# fun unsafe behaviour_path_get_path as ^
       { withBehaviourPath* `BehaviourPath' } -> `Path' newPath* #}
behaviourPathPath :: Attr BehaviourPath Path
behaviourPathPath = newAttr behaviourPathGetPath behaviourPathSetPath

