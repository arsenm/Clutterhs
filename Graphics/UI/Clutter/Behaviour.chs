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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- | Class for providing behaviours to actors
module Graphics.UI.Clutter.Behaviour (
-- * Description

-- | 'Behaviour' is the base class for implementing behaviours. A
--   behaviour is a controller object for Actors; you can use a
--   behaviour to control one or more properties of an actor (such as
--   its opacity, or its position). A 'Behaviour' is driven by an
--   "alpha function" stored inside a 'Alpha' object; an alpha
--   function is a function depending solely on time. The alpha
--   function computes a value which is then applied to the properties
--   of the actors driven by a behaviour.
--
--   Clutter provides some pre-defined behaviours, like
--   'BehaviourPath', which controls the position of a set of actors
--   making them "walk" along a set of nodes; 'BehaviourOpacity',
--   which controls the opacity of a set of actors; 'BehaviourScale',
--   which controls the width and height of a set of actors.
--
--   To visualize the effects of different alpha functions on a
--   'Behaviour' implementation it is possible to take the
--   'BehaviourPath' as an example:
--
-- * Figure 4. Effects of alpha functions on a path
-- <<file:///home/matt/src/clutterhs/doc/path-alpha-func.png>>
--
--  The actors position between the path's end points directly
--  correlates to the ClutterAlpha's current alpha value driving the
--  behaviour. With the ClutterAlpha's function set to 'AlphaRampInc'
--  the actor will follow the path at a constant velocity, but when
--  changing to 'AlphaSineInc' the actor initially accelerates before
--  quickly decelerating.
--
--  In order to implement a new behaviour you should subclass
--  'Behaviour' and override the "alpha_notify" virtual function;
--  inside the overridden function you should obtain the alpha value
--  from the 'Alpha' instance bound to the behaviour and apply it
--  to the desiderd property (or properties) of every actor controlled
--  by the behaviour.
--
-- 'Behaviour' is available since Clutter 0.2
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Behaviour'
-- |         +----'BehaviourDepth'
-- |         +----'BehaviourEllipse'
-- |         +----'BehaviourOpacity'
-- |         +----'BehaviourPath'
-- |         +----'BehaviourRotate'
-- |         +----'BehaviourScale'
-- @

-- * Methods
  behaviourApply,
  behaviourRemove,
  behaviourRemoveAll,
  behaviourIsApplied,
  behaviourActorsForeach,
  behaviourGetActors, --Set actors??
  behaviourGetNActors,
  behaviourGetNthActor,
  behaviourGetAlpha,
  behaviourSetAlpha,

-- * Attributes
  behaviourAlpha,

-- * Signals
  onApplied,
  afterApplied,
  applied,
  onRemoved,
  afterRemoved,
  removed
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.Attributes

-- | Applies behave to actor. This function adds a reference on the actor.
--
-- [@behave@] a Behaviour
--
-- [@actor@] an Actor
--
-- * Since 0.2
--
{# fun unsafe behaviour_apply as ^ `(BehaviourClass behave, ActorClass actor)' =>
       { withBehaviourClass* `behave', withActorClass* `actor' } -> `()' #}


-- | Removes actor from the list of Actors to which behave
--   applies. This function removes a reference on the actor.
--
-- [@behave@] a Behaviour
--
-- [@actor@] an Actor
--
-- * Since 0.2
--
{# fun unsafe behaviour_remove as ^ `(BehaviourClass behave, ActorClass actor)' =>
       { withBehaviourClass* `behave', withActorClass* `actor' } -> `()' #}


-- | Removes every actor from the list that behave holds.
--
-- [@behave@] a Behaviour
--
-- * Since 0.4
--
{# fun unsafe behaviour_remove_all as ^ `(BehaviourClass behave)' =>
       { withBehaviourClass* `behave' } -> `()' #}


-- | Check if behave applied to actor.
--
-- [@behave@] a Behaviour
--
-- [@actor@] an Actor
--
-- [@Returns@] @True@ if actor has behaviour. @False@ otherwise.
--
-- * Since 0.4
--
{# fun unsafe behaviour_is_applied as ^ `(BehaviourClass behave, ActorClass actor)' =>
       { withBehaviourClass* `behave', withActorClass* `actor' } -> `Bool' #}


-- | Calls func for every actor driven by behave.
--
-- [@behave@] a Behaviour
--
-- [@func@] a function called for each actor
--
-- * Since 0.2
--
behaviourActorsForeach :: (BehaviourClass behave) => behave -> BehaviourForeachFunc -> IO ()
behaviourActorsForeach b func = withBehaviourClass b $ \bptr -> do
                                funcPtr <- newBehaviourForeachFunc func
                                {# call unsafe behaviour_actors_foreach #} bptr funcPtr nullPtr
                                freeHaskellFunPtr funcPtr
                                --CHECKME: unsafe?

--TODO: We could make a rw attribute even though clutter doesn't do that.
-- | Retrieves all the actors to which behave applies. It is not
--   recommended for derived classes to use this in there alpha notify
--   method but use behaviourActorsForeach as it avoids alot of
--   needless allocations.
--
-- [@behave@] a Behaviour
--
-- [@Returns@] a list of actors to which the behaviour applies.
--
-- * Since 0.2
--
{# fun unsafe behaviour_get_actors as ^
       `(BehaviourClass behave)' => { withBehaviourClass* `behave' } -> `[Actor]' newActorList* #}

-- | Gets an actor the behaviour was applied to referenced by index
--   num.
--
-- [@behave@] a Behaviour
--
-- [@index@] the index of an actor to which this behaviour is applied.
--
-- [@Returns@] an @Just@ an Actor, or @Nothing@ if index is invalid.
--
-- * Since 0.2
--
{# fun unsafe behaviour_get_nth_actor as ^
       `(BehaviourClass behave)' => { withBehaviourClass* `behave', `Int' } -> `Maybe Actor' maybeNewActor* #}

-- | Gets the number of actors this behaviour is applied too.
--
-- [@behave@] a Behaviour
--
-- [@Returns@]  The number of applied actors
--
-- * Since 0.2
--
{# fun unsafe behaviour_get_n_actors as ^
       `(BehaviourClass behave)' => { withBehaviourClass* `behave' } -> `Int' #}

-- | Retrieves the 'Alpha' object bound to behave.
--
-- [@behave@] a 'Behaviour'
--
-- [@Returns@] @Just@ The Alpha or @Nothing@
--
-- * Since 0.2
--
{# fun unsafe behaviour_get_alpha as ^
       `(BehaviourClass behave)' => { withBehaviourClass* `behave' } -> `Maybe Alpha' maybeNewAlpha* #}

-- | Binds alpha to a Behaviour. The 'Alpha' object is what makes a
--   behaviour work: for each tick of the timeline used by 'Alpha' a
--   new value of the alpha parameter is computed by the alpha
--   function; the value should be used by the 'Behaviour' to update
--   one or more properties of the actors to which the behaviour
--   applies.
--
-- | If alpha is not @Nothing@, the Behaviour will take ownership of
--  the 'Alpha' instance.
--
-- [@behave@] a Behaviour
--
-- [@alpha@] @Just@ an Alpha or @Nothing@ to unset a previously set α
--
-- * Since 0.2
--
{# fun unsafe behaviour_set_alpha as ^
       `(BehaviourClass behave)' => { withBehaviourClass* `behave', withMaybeAlpha* `Maybe Alpha' } -> `()' #}



-- | The 'Α' object used to drive this behaviour. An 'Alpha' object
--   binds a 'Timeline' and a function which computes a value (the
--   "alpha") depending on the time. Each time the alpha value changes
--   the alpha-notify virtual function is called.
--
-- * Since 0.2
--
behaviourAlpha :: (BehaviourClass self) => Attr self (Maybe Alpha)
behaviourAlpha = newNamedAttr "alpha" behaviourGetAlpha behaviourSetAlpha

onApplied, afterApplied :: (BehaviourClass behave) => behave -> (Actor -> IO ()) -> IO (ConnectId behave)
onApplied = connect_OBJECT__NONE "applied" False
afterApplied = connect_OBJECT__NONE "applied" True


-- | The ::apply signal is emitted each time the behaviour is applied to an actor.
--
--  [@actor@] the actor the behaviour was applied to.
--
-- * Since 0.4
--
applied :: (BehaviourClass behave) => Signal behave (Actor ->IO ())
applied = Signal (connect_OBJECT__NONE "applied")


onRemoved, afterRemoved :: (BehaviourClass behave) => behave -> (Actor -> IO ()) -> IO (ConnectId behave)
onRemoved = connect_OBJECT__NONE "removed" False
afterRemoved = connect_OBJECT__NONE "removed" True


-- | The ::removed signal is emitted each time a behaviour is not
--   applied to an actor anymore.
--
-- [@actor@] the removed actor
--
removed :: (BehaviourClass behave) => Signal behave (Actor ->IO ())
removed = Signal (connect_OBJECT__NONE "removed")

