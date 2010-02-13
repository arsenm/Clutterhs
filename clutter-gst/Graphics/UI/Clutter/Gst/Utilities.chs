-- -*-haskell-*-
--  ClutterGst Utilities
--
--  Author : Matthew Arsenault
--
--  Created: 13 Feb 2010
--
--  Copyright (C) 2010 Matthew Arsenault
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
#include <clutter-gst/clutter-gst.h>

{# context lib="clutter_gst" prefix="clutter_gst" #}

-- | Utilities — Utility functions for Clutter-Gst.
module Graphics.UI.Clutter.Gst.Utilities (
  clutterGstInit
  ) where

import C2HS
import Control.Monad
import System.Glib.UTFString
import System.Environment (getArgs, getProgName)
import Graphics.UI.Clutter (InitError(..))

--TODO: Check threading

-- | Utility function to initialize both Clutter and GStreamer.
--
-- This function should be called before calling any other GLib
-- functions.
--
clutterGstInit :: IO InitError
clutterGstInit = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen addrs $ \argc argv ->
    with argv $ \argvp ->
    with argc $ \argcp -> do
                res <- liftM cToEnum $ {# call unsafe init #} (castPtr argcp) (castPtr argvp)
                when (res /= InitSuccess) $ error ("Cannot initialize ClutterGst." ++ show res)
                return res


