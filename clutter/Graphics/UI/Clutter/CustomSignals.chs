{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--
--  Author : Matthew Arsenault
--
--  Created: 20 November 2009
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
-- #hide

--I needed signal handlers with checks for null, but I was too lazy to
--modify the generator.
module Graphics.UI.Clutter.CustomSignals (
  connect_MAYBEOBJECT__NONE,
  connect_BOXED_FLAGS__NONE
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError (failOnGError)
import System.Glib.Signals
import System.Glib.GObject

{#context lib="clutter" prefix="clutter" #}

connect_MAYBEOBJECT__NONE ::
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (Maybe a' -> IO ()) ->
  IO (ConnectId obj)
connect_MAYBEOBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 = if obj1 == nullPtr
                           then failOnGError (user Nothing)
                           else failOnGError $
                                  makeNewGObject mkGObject (return obj1) >>= \obj1' ->
                                    user (Just (unsafeCastGObject obj1'))




--The generator was producing slightly wrong stuff here, but I'm too
--lazy to fix it right now.
connect_BOXED_FLAGS__NONE ::
  (Flags b, GObjectClass obj) => SignalName ->
  (Ptr a' -> IO a) ->
  ConnectAfter -> obj ->
  (a -> [b] -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_FLAGS__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Int -> IO ()
        action _ box1 flags2 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' (toFlags flags2)


