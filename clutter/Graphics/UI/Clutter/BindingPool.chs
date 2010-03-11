-- -*-haskell-*-
--  Clutter BindingPool
--
--  Author : Matthew Arsenault
--
--  Created: 11 Sep 2009
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

-- | Key Bindings — Pool for key bindings
module Graphics.UI.Clutter.BindingPool (
-- * Description
-- | 'BindingPool' is a data structure holding a set of key
-- bindings. Each key binding associates a key symbol (eventually with
-- modifiers) to an action. A callback function is associated to each
-- action.
--
-- For a given key symbol and modifier mask combination there can be
-- only one action; for each action there can be only one
-- callback. There can be multiple actions with the same name, and the
-- same callback can be used to handle multiple key bindings.
--
--
-- The callback has a signature of :: GObjectClass obj => obj \->
-- String \-> KeyVal \-> [ModifierType] \-> IO Bool The actor should then
-- override the 'keyPressEvent' and use 'bindingPoolActivate' to
-- match a 'KeyEvent' structure to one of the actions:
--
-- @
--
--   {-  Activate any callback matching the key symbol and modifiers
--    mask of the key event. The returned value can be directly
--    used to signal that the actor has handled the event.   -}
--
--   actor \`on\` keyPressEvent $ do modifiers \<\- eventState
--                              keyval    \<\- eventKeyCode
--                              liftIO \(bindingPoolActivate pool actor keyval modifiers\)
-- @
--
-- The 'bindingPoolActivate' function will return @False@ if no action
-- for the given key binding was found, if the action was blocked
-- \(using 'bindingPoolBlockAction'\) or if the key binding handler
-- returned @False@.
--
-- 'BindingPool' is available since Clutter 1.0
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
--  |   +----'BindingPool'
-- @

-- * Types
  BindingPool,
  KeyVal,
  BindingActionFunc,

-- * Constructors
  bindingPoolNew,

-- * Methods
--bindingPoolGetForClass,
  bindingPoolFind,
  bindingPoolInstallAction,
  bindingPoolOverrideAction,
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

bindingPoolInstallAction :: GObjectClass a => BindingPool -> String -> KeyVal -> [ModifierType] -> BindingActionFunc a -> IO ()
bindingPoolInstallAction bp name keyval modif cb = withBindingPool bp $ \bpPtr ->
                                                     withCString name $ \namePtr ->
                                                       let func = {# call binding_pool_install_action #}
                                                           mod = cFromModFlags modif
                                                           kc = cIntConv keyval
                                                       in do
                                                            cbPtr <- newBindingActionFunc cb
                                                            func bpPtr namePtr kc mod (castFunPtr cbPtr) (castFunPtrToPtr cbPtr) destroyFunPtr


bindingPoolOverrideAction :: GObjectClass a => BindingPool -> KeyVal -> [ModifierType] -> BindingActionFunc a -> IO ()
bindingPoolOverrideAction bp keyval modif cb = withBindingPool bp $ \bpPtr ->
                                                 let func = {# call binding_pool_override_action #}
                                                     mod = cFromModFlags modif
                                                     kc = cIntConv keyval
                                                 in do
                                                   cbPtr <- newBindingActionFunc cb
                                                   func bpPtr kc mod (castFunPtr cbPtr) (castFunPtrToPtr cbPtr) destroyFunPtr

{# fun unsafe binding_pool_find_action as ^
       { withBindingPool* `BindingPool', cIntConv `KeyVal', cFromModFlags `[ModifierType]' } -> `Maybe String' maybeString* #}


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
{# fun binding_pool_activate as ^ `(GObjectClass gobject)' =>
       { withBindingPool* `BindingPool',
         cIntConv `KeyVal',
         cFromModFlags `[ModifierType]',
         withGObject* `gobject' } -> `Bool' #}

