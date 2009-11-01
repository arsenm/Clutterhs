-- -*-haskell-*-
--  Stuff used internally
--
--  Author : Matthew Arsenault
--
--  Created: 23 Oct 2009
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

module Graphics.UI.Clutter.Utility (
                                    tup2ToF,
                                    tup3ToF,
                                    tup4ToF,

                                    cFromFlags,
                                    cToFlags,

                                    newCairo,
                                    withCairo,
                                    withCairoPath,

                                    peekNFree,
                                    peekNFreeString
                                   ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.Flags
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types (Cairo(..), unCairo)
import qualified Graphics.Rendering.Cairo.Types as Cairo


--There's Probably a better way to do this I'm using this for related
--attributes that you might want to set at once rather than
--separately, e.g. instead of setting X position and Y position, set
--position (X,Y) at the same time
tup2ToF f item (a,b) = f item a b
tup3ToF f item (a,b,c) = f item a b c
tup4ToF f item (a,b,c,d) = f item a b c d

--flag functions from gtk2hs with c int conversion

cToFlags :: (Flags a) => CInt ->  [a]
cToFlags = toFlags . cIntConv

cFromFlags :: (Flags a) => [a] -> CInt
cFromFlags = cIntConv . fromFlags

--convenient marshalling not provided by gtk2hs
newCairo = Cairo . castPtr
withCairoPath = castPtr . Cairo.unPath
withCairo = castPtr . unCairo

peekNFree :: (Storable a) => Ptr a -> IO a
peekNFree p = do
          ret <- peek p
          free p
          return ret

peekNFreeString :: Ptr CChar -> IO String
peekNFreeString p = do
                ret <- peekCString p
                free p
                return ret

