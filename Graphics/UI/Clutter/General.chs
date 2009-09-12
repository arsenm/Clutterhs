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
                                    clutterInit,
                                    clutterMain,
                                    clutterMainQuit,
                                   ) where

{#import Graphics.UI.Clutter.Types#}

import C2HS
import System.Glib.GObject
import System.Glib.UTFString
import System.Environment (getProgName, getArgs)
import Control.Monad (liftM, mapM, when)

--main :: IO ()
--main  = {#call main#}

{#fun unsafe main as clutterMain {} -> `()' #}
{#fun unsafe main_quit as clutterMainQuit {} -> `()' #}

--why do I need this?
pants::CInt -> IO InitError
pants = return . cToEnum . cIntConv

--FIXME: use of id as marshaller seems horribly wrong
{# fun unsafe clutter_init as secretClutterInit {id `Ptr CInt', id `Ptr (Ptr (CString))'} -> `InitError' pants* #}

--just make it go for now. I think some issues with it
--out args? do they matter? also how to use out marshallers correctly
clutterInit :: IO InitError
clutterInit = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen addrs   $ \argc argv ->
    with argv $ \argvp ->
    with argc $ \argcp -> do
--              res <- {#call unsafe init#} (castPtr argcp) (castPtr argvp)
                res <- secretClutterInit (castPtr argcp) (castPtr argvp)
                when (res /= InitSuccess) $ error ("Cannot initialize Clutter." ++ show res)
                return res


