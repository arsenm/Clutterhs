-- -*-haskell-*-
--  Clutter Behaviour
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Behaviour (
                                      behaviourApply,
                                      behaviourRemove,
                                      behaviourRemoveAll,
                                      behaviourIsApplied,
                                    --behaviourActorsForeach, --Just use mapM?
                                    --behaviourGetActors, --Set actors??
                                    --behaviourGetNActors,
                                      behaviourGetNthActor,
                                    --behaviourGetAlpha,
                                    --behaviourSetAlpha,
                                    --behaviourAlpha
                                     ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe behaviour_apply as ^
       `(ActorClass a)' => { withBehaviour* `Behaviour', withActorClass* `a' } -> `()' #}

{# fun unsafe behaviour_remove as ^
       `(ActorClass a)' => { withBehaviour* `Behaviour', withActorClass* `a' } -> `()' #}

{# fun unsafe behaviour_remove_all as ^ { withBehaviour* `Behaviour' } -> `()' #}

{# fun unsafe behaviour_is_applied as ^
       `(ActorClass a)' => { withBehaviour* `Behaviour', withActorClass* `a' } -> `Bool' #}

--{# fun unsafe behaviour_actors_foreach as ^
--       { withBehaviour* `Behaviour', `BehaviourForeachFunc', UserDataToIgnoreSomehow } -> `()' #}

{# fun unsafe behaviour_get_nth_actor as ^ { withBehaviour* `Behaviour', `Int' } -> `Actor' newActor* #}

{-
{# fun unsafe behaviour_get_alpha as ^ { withBehaviour* `Behaviour' } -> `Alpha' newAlpha* #}
{# fun unsafe behaviour_set_alpha as ^ { withBehaviour* `Behaviour', withAlpha* `Alpha' } -> `()' #}
behaviourAlpha :: Attr Behaviour Alpha
behaviourAlpha = newAttr behaviourGetAlpha behaviourSetAlpha
-}

