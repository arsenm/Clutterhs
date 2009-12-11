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

-- | BehaviourPath — A behaviour for moving actors along a 'Path'
module Graphics.UI.Clutter.BehaviourPath (
-- |
-- @
-- |  'GObject'
-- |    +----'Behaviour'
-- |           +----'BehaviourPath'
-- |
-- @

-- * Types
  BehaviourPath,
  BehaviourPathClass,
  Knot,

-- * Constructors
  behaviourPathNew,
  behaviourPathNewWithDescription,
  behaviourPathNewWithKnots,

-- * Methods

  behaviourPathSetPath,
  behaviourPathGetPath,

-- * Attributes
  behaviourPathPath,

-- * Signals
  onKnotReached,
  afterKnotReached,
  knotReached
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}

import C2HS
import System.Glib.Attributes

{# pointer *Knot as KnotPtr foreign -> Knot nocode #}

{# fun unsafe behaviour_path_new as ^
       { withAlpha* `Alpha', withPath* `Path' } -> `BehaviourPath' newBehaviourPath* #}
{# fun unsafe behaviour_path_new_with_description as ^
       { withAlpha* `Alpha', `String' } -> `BehaviourPath' newBehaviourPath* #}

behaviourPathNewWithKnots :: Alpha -> [Knot] -> IO BehaviourPath
behaviourPathNewWithKnots alp knots = let func = {# call unsafe behaviour_path_new_with_knots #}
                                      in withAlpha alp $ \aptr ->
                                          withArrayLen knots $ \len knotptr ->
                                          newBehaviourPath =<< func aptr knotptr (cIntConv len)


{# fun unsafe behaviour_path_set_path as ^
       { withBehaviourPath* `BehaviourPath', withPath* `Path'} -> `()' #}
{# fun unsafe behaviour_path_get_path as ^
       { withBehaviourPath* `BehaviourPath' } -> `Path' newPath* #}


-- Attributes

behaviourPathPath :: Attr BehaviourPath Path
behaviourPathPath = newNamedAttr "path" behaviourPathGetPath behaviourPathSetPath


onKnotReached, afterKnotReached :: BehaviourPath -> (Word -> IO ()) -> IO (ConnectId BehaviourPath)
onKnotReached = connect_WORD__NONE "knot-reached" False
afterKnotReached = connect_WORD__NONE "knot-reached" True


-- | This signal is emitted each time a node defined inside the path
--   is reached.
--
--  [@knot_num@] the index of the 'PathKnot' reached
--
-- * Since 0.2
--
knotReached :: Signal BehaviourPath (Word -> IO ())
knotReached = Signal (connect_WORD__NONE "knot_reached")


