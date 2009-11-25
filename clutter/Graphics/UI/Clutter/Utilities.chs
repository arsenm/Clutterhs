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
import System.Glib.GObject (DestroyNotify, mkFunPtrDestroyNotify)


{# fun pure unsafe util_next_p2 as ^ { `Int' } -> `Int' #}

{-
{# fun unsafe timeout_pool_new as ^ { `Priority' } -> `TimeoutPool' newTimeoutPool #}

timeoutPoolAdd :: TimeoutPool -> Word -> SourceFunc -> IO HandlerID
timeoutPoolAdd pool fps cb = let func = {# call unsafe timeout_pool_add #}  --CHECKME: unsafe?
                             in withTimeoutPool pool $ \poolPtr -> do
                                  (sf, gdestrnotify) <- makeCallback cb
                                  func poolPtr (cIntConv fps) sf nullPtr gdestrnotify

{# fun unsafe timeout_pool_remove as ^ { withTimeoutPool `TimeoutPool', cIntConv `Word' } -> `()' #}
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
  dPtr <- mkFunPtrDestroyNotify funPtr
  return (funPtr, dPtr)

frameSourceAdd :: Word -> SourceFunc -> IO HandlerID
frameSourceAdd fps sfFunc = let func = {# call unsafe frame_source_add #}
                            in do
                              sf <- mkSourceFunc sfFunc
                              liftM cIntConv $ func (cIntConv fps) sf nullPtr

frameSourceAddFull :: Priority -> HandlerID -> SourceFunc -> IO HandlerID
frameSourceAddFull priority fps sfFunc = let func = {# call unsafe frame_source_add_full #}
                                         in do
                                           (sf, gdn) <- makeCallback sfFunc
                                           liftM cIntConv $ func (cIntConv priority) (cIntConv fps) sf nullPtr gdn


--TODO: Other stuff here needs cogl



