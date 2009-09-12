-- -*-haskell-*-
--  Clutter General
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.General (
                                    clutterInit
                                   ) where

{#import Graphics.UI.Clutter.Types#}

import C2HS
import System.Glib.GObject
import System.Glib.UTFString
import System.Environment (getProgName, getArgs)
import Control.Monad (liftM, mapM, when)


mainQuit :: IO ()
mainQuit  = {#call main_quit#}

--just make it go for now. I think some issues with it
clutterInit :: IO InitError
clutterInit = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen addrs   $ \argc argv ->
    with argv $ \argvp ->
    with argc $ \argcp -> do
                res <- {#call unsafe init#} (castPtr argcp) (castPtr argvp)
                when (cToEnum res /= InitSuccess) $ error ("Cannot initialize Clutter." ++ show res)
                return (cToEnum res)

