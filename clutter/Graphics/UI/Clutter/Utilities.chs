-- -*-haskell-*-
--  Utilities
--
--  Author : Matthew Arsenault
--
--  Created: 1 Nov 2009
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
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}
{# context lib="glib" prefix="g" #}

--CHECKME: Do I want to have the functions that claim to be not for applications?

module Graphics.UI.Clutter.Utilities (
-- * Types
--TimeoutPool,
  SourceFunc,
  HandlerID,

  utilNextP2,
--timeoutPoolNew,
--timeoutPoolAdd,
  frameSourceAdd,
  frameSourceAddFull
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.MainLoop
import System.Glib.GObject


{# fun pure unsafe util_next_p2 as ^ { `Int' } -> `Int' #}

{-
{# pointer *ClutterTimeoutPool as TimeoutPool foreign newtype #}


-- the newly created ClutterTimeoutPool. The created pool is owned by
-- the GLib default context and will be automatically destroyed when
-- the context is destroyed. It is possible to force the destruction
-- of the timeout pool using g_source_destroy()

newTimeoutPool :: Ptr TimeoutPool -> IO TimeoutPool
newTimeoutPool = liftM TimeoutPool . newForeignPtr_


--CHECKME: Should these even be bound?


-- | Creates a new timeout pool source. A timeout pool should be used
-- when multiple timeout functions, running at the same priority, are
-- needed and the g_timeout_add() API might lead to starvation of the
-- time slice of the main loop. A timeout pool allocates a single time
-- slice of the main loop and runs every timeout function inside
-- it. The timeout pool is always sorted, so that the extraction of
-- the next timeout function is a constant time operation.
--
-- Inside Clutter, every 'Timeline' shares the same timeout pool,
-- unless the CLUTTER_TIMELINE=no-pool environment variable is set.
--
-- 'TimeoutPool' is part of the 'Timeline' implementation and should
-- not be used by application developers.
--
-- [@priority@] the priority of the timeout pool. Typically this will
-- be 'priorityDefault'
--
-- [@Returns@] the newly created 'TimeoutPool'. The created pool is
-- owned by the GLib default context and will be automatically
-- destroyed when the context is destroyed. It is possible to force
-- the destruction of the timeout pool using g_source_destroy()
--
-- * Since 0.4
--
{# fun unsafe timeout_pool_new as ^ { cIntConv `Priority' } -> `TimeoutPool' newTimeoutPool* #}


timeoutPoolAdd :: TimeoutPool -> Word -> SourceFunc -> IO HandlerID
timeoutPoolAdd pool fps cb = let func = {# call unsafe timeout_pool_add #}  --CHECKME: unsafe?
                             in withTimeoutPool pool $ \poolPtr -> do
                                  (sf, gdestrnotify) <- makeCallback cb
                                  ret <- func poolPtr (cIntConv fps) sf nullPtr gdestrnotify
                                  return (cIntConv ret)

{# fun unsafe timeout_pool_remove as ^ { withTimeoutPool* `TimeoutPool', cIntConv `Word' } -> `()' #}

-}


--CHECKME: Um, says a wrapper around frame_source_add_full but doesn't take gdestroynotify?
--leak?
--ALSOCHECKME: unsafe?


--this stuff is in gtk2hs but not exported
type SourceFunc = IO Int
type HandlerID = Word

{# pointer SourceFunc as CSourceFunc #}

foreign import ccall "wrapper" mkSourceFunc :: SourceFunc -> IO CSourceFunc

makeCallback :: SourceFunc -> IO (CSourceFunc, DestroyNotify)
makeCallback fun = do
  funPtr <- mkSourceFunc fun
  return (funPtr, destroyFunPtr)




-- | Simple wrapper around 'frameSourceAddFull'.
--
-- [@fps@] the number of times per second to call the function
--
-- [@func@] function to call
--
-- [@Returns@] the ID (greater than 0) of the event source.
--
-- * Since 0.8
--
frameSourceAdd :: Word -> SourceFunc -> IO HandlerID
frameSourceAdd fps sfFunc = let func = {# call unsafe frame_source_add #}
                            in do
                              sf <- mkSourceFunc sfFunc
                              liftM cIntConv $ func (cIntConv fps) sf nullPtr



-- | Sets a function to be called at regular intervals with the given
-- priority. The function is called repeatedly until it returns
-- @False@, at which point the timeout is automatically destroyed and
-- the function will not be called again. The notify function is
-- called when the timeout is destroyed. The first call to the
-- function will be at the end of the first interval.
--
-- This function is similar to 'timeoutAddFull' except that it will
-- try to compensate for delays. For example, if func takes half the
-- interval time to execute then the function will be called again
-- half the interval time after it finished. In contrast
-- 'timeoutAddFull' would not fire until a full interval after the
-- function completes so the delay between calls would be 1.0 / fps *
-- 1.5. This function does not however try to invoke the function
-- multiple times to catch up missing frames if func takes more than
-- interval ms to execute.
--
-- [@priority@] the priority of the frame source. Typically this will
-- be in the range between 'priorityDefault' and 'priorityHigh'.
--
-- [@fps@] the number of times per second to call the function
--
-- [@func@] function to call
--
-- [@Returns@] the ID (greater than 0) of the event source.
--
-- * Since 0.8
--
frameSourceAddFull :: Priority -> HandlerID -> SourceFunc -> IO HandlerID
frameSourceAddFull priority fps sfFunc = let func = {# call unsafe frame_source_add_full #}
                                         in do
                                           (sf, gdn) <- makeCallback sfFunc
                                           liftM cIntConv $ func (cIntConv priority) (cIntConv fps) sf (castFunPtrToPtr sf) gdn


--TODO: Other stuff here needs cogl



