-- -*-haskell-*-
--  Clutter BindingPool
--
--  Author : Matthew Arsenault
--
--  Created: 11 Sep 2009
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.BindingPool (
-- * Description

-- | BindingPool is a data structure holding a
-- set of key bindings. Each key binding associates a key symbol
-- (eventually with modifiers) to an action. A callback function is
-- associated to each action.
--
-- For a given key symbol and modifier mask combination there can be
-- only one action; for each action there can be only one
-- callback. There can be multiple actions with the same name, and the
-- same callback can be used to handle multiple key bindings.
--
-- Actors requiring key bindings should create a new
-- 'BindingPool' inside their class initialization function and
-- then install actions like this:
--
-- TODO: Replace this
-- @
-- static void
-- foo_class_init (FooClass *klass)
-- {
--   ClutterBindingPool *binding_pool;
--
--   binding_pool = clutter_binding_pool_get_for_class (klass);
--
--   clutter_binding_pool_install_action (binding_pool, "move-up",
--                                        CLUTTER_Up, 0,
--                                        G_CALLBACK (foo_action_move_up),
--                                        NULL, NULL);
--   clutter_binding_pool_install_action (binding_pool, "move-up",
--                                        CLUTTER_KP_Up, 0,
--                                        G_CALLBACK (foo_action_move_up),
--                                        NULL, NULL);
-- }
-- The callback has a signature of:
--
--    gboolean (* callback) (GObject             *instance,
--                           const gchar         *action_name,
--                           guint                key_val,
--                           ClutterModifierType  modifiers,
--                           gpointer             user_data);
--
-- The actor should then override the \"key-press-event\" and use
-- 'bindingPoolActivate' to match a 'KeyEvent'
-- structure to one of the actions:
--
--   ClutterBindingPool *pool;
--
--   /* retrieve the binding pool for the type of the actor */
--   pool = clutter_binding_pool_find (G_OBJECT_TYPE_NAME (actor));
--
--   /* activate any callback matching the key symbol and modifiers
--    * mask of the key event. the returned value can be directly
--    * used to signal that the actor has handled the event.
--    */
--   return clutter_binding_pool_activate (pool, G_OBJECT (actor),
--                                         key_event->keyval,
--                                         key_event->modifier_state);
--
-- @
-- The 'bindingPoolActivate' function will return @False@ if no action
-- for the given key binding was found, if the action was blocked
-- (using 'bindingPoolBlockAction') or if the key binding handler
-- returned @False@.
--
-- 'BindingPool' is available since Clutter 1.0
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'BindingPool'
-- @

-- * Types
  BindingPool,
--BindingActionFunc,

-- * Constructors
  bindingPoolNew,

-- * Methods
--bindingPoolGetForClass,
  bindingPoolFind,
  bindingPoolInstallAction,
--bindingPoolInstallClosure,
  bindingPoolOverrideAction,
--bindingPoolOverrideClosure,
  bindingPoolFindAction,
  bindingPoolRemoveAction,
  bindingPoolBlockAction,
  bindingPoolUnblockAction,
  bindingPoolActivate
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.GObject
import System.Glib.FFI


type KeyVal = Word

--TODO: Private, don't document
maybeBindingPool = maybeNullNew newBindingPool


-- | Creates a new 'BindingPool' that can be used to store key
--   bindings for an actor. The name must be a unique identifier for
--   the binding pool, so that 'bindingPoolFind' will be able to
--   return the correct binding pool.
--
-- [@name@] the name of the binding pool
--
-- [@Returns@] the newly created binding pool with the given name.
--
-- * Since 1.0
--
{# fun unsafe binding_pool_new as ^ { `String' } -> `BindingPool' newBindingPool* #}

--I don't understand this function. Looks dirty though.
--{# fun unsafe binding_pool_get_for_class as ^ {} -> `BindingPool' newBindingPool* #}



{# fun unsafe binding_pool_find as ^ { `String' } -> `Maybe BindingPool' maybeBindingPool* #}

bindingPoolInstallAction :: BindingPool -> String -> KeyVal -> [ModifierType] -> GCallback -> IO ()
bindingPoolInstallAction bp name keyval modif gCB = withBindingPool bp $ \bpPtr ->
                                                    withCString name $ \namePtr ->
                                                    --CHECKME: unsafe? Most likely safe, callback
                                                    let func = {# call binding_pool_install_action #}
                                                        mod = cFromModFlags modif
                                                        kc = cIntConv keyval
                                                    in do
                                                      gcbPtr <- newGCallback gCB
                                                      gdestroy <- mkFunPtrDestroyNotify gcbPtr
                                                      func bpPtr namePtr kc mod gcbPtr nullPtr gdestroy


bindingPoolOverrideAction :: BindingPool -> KeyVal -> [ModifierType] -> GCallback -> IO ()
bindingPoolOverrideAction bp keyval modif gCB = withBindingPool bp $ \bpPtr ->
                                                --CHECKME: unsafe? Most likely safe, callback
                                                let func = {# call binding_pool_override_action #}
                                                    mod = cFromModFlags modif
                                                    kc = cIntConv keyval
                                                in do
                                                  gcbPtr <- newGCallback gCB
                                                  gdestroy <- mkFunPtrDestroyNotify gcbPtr
                                                  func bpPtr kc mod gcbPtr nullPtr gdestroy

{# fun unsafe binding_pool_find_action as ^
       { withBindingPool* `BindingPool', cIntConv `KeyVal', cFromModFlags `[ModifierType]' } -> `Maybe String' maybeString* #}


--FIXME: make list of modifiers, cFromFlags won't work since Flags instance won't work for ModifierType
-- | Removes the action matching the given key_val, modifiers pair, if
--   any exists.
--
-- [@pool@] a 'BindingPool'
--
-- [@key_val@] a key symbol
--
-- [@modifiers@] a list of modifiers
--
-- * Since 1.0
--

{# fun unsafe binding_pool_remove_action as ^
       { withBindingPool* `BindingPool', cIntConv `KeyVal', cFromModFlags `[ModifierType]' } -> `()' #}

-- | Blocks all the actions with name action_name inside pool.
--
-- [@pool@] a 'BindingPool'
--
-- [@action_name@] an action name
--
-- * Since 1.0
--
{# fun unsafe binding_pool_block_action as ^
       { withBindingPool* `BindingPool', `String' } -> `()' #}


-- | Unblocks all the actions with name action_name inside pool.
--
-- Unblocking an action does not cause the callback bound to it to be
-- invoked in case 'bindingPoolActivate' was called on an
-- action previously blocked with 'bindingPoolBlockAction'.
--
-- [@pool@] a 'BindingPool'
--
-- [@action_name@] an action name
--
-- * Since 1.0
--
{# fun unsafe binding_pool_unblock_action as ^
       { withBindingPool* `BindingPool', `String' } -> `()' #}


-- | Activates the callback associated to the action that is bound to the key_val and modifiers pair.
--
-- The callback has the following signature:
--
-- @
--   void (* callback) (GObject             *gobject,
--                     const gchar         *action_name,
--                     guint                key_val,
--                     ClutterModifierType  modifiers,
--                     gpointer             user_data);
-- @
--
-- Where the GObject instance is gobject and the user data is the one
-- passed when installing the action with
-- 'bindingPoolInstallAction'.
--
-- If the action bound to the key_val, modifiers pair has been blocked
-- using 'bindingPoolBlockAction', the callback will not be
-- invoked, and this function will return @False@.
--
-- [@pool@] a 'BindingPool'
--
-- [@key_val@] the key symbol
--
-- [@modifiers@] bitmask for the modifiers
--
-- [@gobject@] a 'GObject'
--
-- [@Returns@] @True@ if an action was found and was activated
--
-- * Since 1.0
--

{# fun unsafe binding_pool_activate as ^ `(GObjectClass gobject)' =>
       { withBindingPool* `BindingPool',
         cIntConv `KeyVal',
         cFromModFlags `[ModifierType]',
         withGObject* `gobject' } -> `Bool' #}


