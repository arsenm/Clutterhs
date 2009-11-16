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

  newPangoContext,

  cFromFlags,
  cToFlags,
  toModFlags,
  cFromModFlags,

--TODO: Move some of this stuff to types
  newCairo,
  withCairo,
  withCairoPath,
  withPangoLayoutRaw,

  peekNFree,
  peekNFreeString,
  maybeString,
  withMaybeString,
  maybeNullNew,
  maybeNullPeek,

  withMaybeAlpha,
  withMaybeText,
  withMaybeTimeline,
  withMaybeShader,
  withMaybeColor,
  withMaybeActorClass,
  maybeNewActor,
  maybeNewStage,
  maybeNewAlpha,
  maybeNewAnimation,
  maybeNewTimeline,
  maybeNewTexture,
  maybeNewShader
 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Data.Maybe (catMaybes)

import System.Glib.Flags
import Graphics.Rendering.Cairo.Types (Cairo(..), unCairo)
import qualified Graphics.Rendering.Cairo.Types as Cairo

import Graphics.UI.Gtk.Types (PangoContext, mkPangoContext, unPangoLayoutRaw)

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

{# pointer *cairo_t as CairoPtr foreign -> Cairo nocode #}

--convenient marshalling not provided by gtk2hs
newCairo :: Ptr Cairo -> Cairo
newCairo = Cairo

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


maybeNewActor :: Ptr Actor -> IO (Maybe Actor)
maybeNewActor = maybeNullNew newActor

maybeNewStage :: Ptr Actor -> IO (Maybe Stage)
maybeNewStage = maybeNullNew newStage

maybeNewAlpha :: Ptr Alpha -> IO (Maybe Alpha)
maybeNewAlpha = maybeNullNew newAlpha

maybeNewAnimation :: Ptr Animation -> IO (Maybe Animation)
maybeNewAnimation = maybeNullNew newAnimation

maybeNewTimeline :: Ptr Timeline -> IO (Maybe Timeline)
maybeNewTimeline = maybeNullNew newTimeline

maybeNewTexture :: Ptr Actor -> IO (Maybe Texture)
maybeNewTexture = maybeNullNew newTexture

maybeNewShader :: Ptr Shader -> IO (Maybe Shader)
maybeNewShader = maybeNullNew newShader


-- e.g. maybeNewRectangle = maybeNullNew newRectangle
maybeNullNew :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNullNew marshal ptr = do
  if ptr == nullPtr
    then return Prelude.Nothing
    else marshal ptr >>= return . Just

maybeNullPeek :: (Storable a) => Ptr a -> IO (Maybe a)
maybeNullPeek = maybeNullNew peek


maybeString :: Ptr CChar -> IO (Maybe String)
maybeString ptr = do
  if ptr == nullPtr
    then return Prelude.Nothing
    else peekCString ptr >>= return . Just

withMaybeString :: Maybe String -> (Ptr CChar -> IO a) -> IO a
withMaybeString Prelude.Nothing act = act nullPtr
withMaybeString (Just str) act = withCString str act

withMaybeAlpha = maybeWith withAlpha
withMaybeText = maybeWith withText
withMaybeTimeline = maybeWith withTimeline
withMaybeShader = maybeWith withShader
withMaybeColor = maybeWith withColor

withMaybeActorClass :: (ActorClass a) => Maybe a -> (Ptr Actor -> IO b) -> IO b
withMaybeActorClass = maybeWith withActorClass


cFromModFlags :: [ModifierType] -> CInt
cFromModFlags = cIntConv . fromModFlags

--can't just make instance of flags for this, since toModFlags must be different
fromModFlags :: [ModifierType] -> Int
fromModFlags is = cIntConv (orNum 0 is)
  where orNum n []     = n
        orNum n (i:is) = orNum (n .|. fromEnum i) is


--The normal one from gtk2hs does not work here. there is a
--discontinuity in the enum of unused bits and also an internally used
--bit, therefore minBound .. maxBound fails, so do this shitty listing
--of all options
toModFlags :: Int -> [ModifierType]
toModFlags n = catMaybes [ if n .&. fromEnum flag == fromEnum flag
                            then Just flag
                            else Prelude.Nothing
                          | flag <- [ShiftMask,
                                     LockMask,
                                     ControlMask,
                                     Mod1Mask,
                                     Mod2Mask,
                                     Mod3Mask,
                                     Mod4Mask,
                                     Mod5Mask,
                                     Button1Mask,
                                     Button2Mask,
                                     Button3Mask,
                                     Button4Mask,
                                     Button5Mask,
                                     SuperMask,
                                     HyperMask,
                                     MetaMask,
                                     ReleaseMask,
                                     ModifierMask]
                         ]


withPangoLayoutRaw = withForeignPtr . unPangoLayoutRaw

newPangoContext :: Ptr PangoContext -> IO PangoContext
newPangoContext p = makeNewObject mkPangoContext (return p)

