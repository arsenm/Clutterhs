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
                                        bindingPoolNew,
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

import C2HS
import System.Glib.GObject

--TODO: GUInt, also move to Types and possibly rename
type KeyVal = Word64

{# fun unsafe binding_pool_new as ^ { `String' } -> `BindingPool' newBindingPool* #}

--
--{# fun unsafe binding_pool_get_for_class as ^ {} -> `BindingPool' newBindingPool* #}

{# fun unsafe binding_pool_find as ^ { `String' } -> `BindingPool' newBindingPool* #}

bindingPoolInstallAction :: BindingPool -> String -> KeyVal -> ModifierType -> GCallback -> IO ()
bindingPoolInstallAction bp name keyval modif gCB = withBindingPool bp $ \bpPtr ->
                                                    withCString name $ \namePtr ->
                                                    --CHECKME: unsafe? Most likely safe, callback
                                                    let func = {# call binding_pool_install_action #}
                                                        mod = cFromEnum modif
                                                        kc = cIntConv keyval
                                                    in do
                                                      gcbPtr <- newGCallback gCB
                                                      gdestroy <- mkFunPtrDestroyNotify gcbPtr
                                                      func bpPtr namePtr kc mod gcbPtr nullPtr gdestroy


bindingPoolOverrideAction :: BindingPool -> KeyVal -> ModifierType -> GCallback -> IO ()
bindingPoolOverrideAction bp keyval modif gCB = withBindingPool bp $ \bpPtr ->
                                                --CHECKME: unsafe? Most likely safe, callback
                                                let func = {# call binding_pool_override_action #}
                                                    mod = cFromEnum modif
                                                    kc = cIntConv keyval
                                                in do
                                                  gcbPtr <- newGCallback gCB
                                                  gdestroy <- mkFunPtrDestroyNotify gcbPtr
                                                  func bpPtr kc mod gcbPtr nullPtr gdestroy

{# fun unsafe binding_pool_find_action as ^
       { withBindingPool* `BindingPool', cIntConv `KeyVal', cFromEnum `ModifierType' } -> `String' #}

{# fun unsafe binding_pool_remove_action as ^
       { withBindingPool* `BindingPool', cIntConv `KeyVal', cFromEnum `ModifierType' } -> `()' #}

{# fun unsafe binding_pool_block_action as ^
       { withBindingPool* `BindingPool', `String' } -> `()' #}

{# fun unsafe binding_pool_unblock_action as ^
       { withBindingPool* `BindingPool', `String' } -> `()' #}


{# fun unsafe binding_pool_activate as ^ `(GObjectClass gobject)' =>
       { withBindingPool* `BindingPool',
         cIntConv `KeyVal',
         cFromEnum `ModifierType',
         withGObject* `gobject' } -> `Bool' #}

