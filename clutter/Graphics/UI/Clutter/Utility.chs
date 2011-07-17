-- -*-haskell-*-
--  Stuff used internally
--
--  Author : Matthew Arsenault
--
--  Created: 23 Oct 2009
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

module Graphics.UI.Clutter.Utility (
  newPangoContext,

  cFromFlags,
  cToFlags,

--TODO: Move some of this stuff to types
  withCairoPath,
  withPangoLayoutRaw,

  peekNFree,
  peekNFreeString,
  peekNFreeMaybeString,
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
  maybeNewTimeline,
  maybeNewTexture,
  maybeNewShader,

  clutterNewAttrFromUIntProperty,
  unsafeCastActor,

#if CLUTTER_CHECK_VERSION(1,2,0)
  unsafeCastLayoutManager,
#endif

  gvPtrToRealValue
 ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.StoreValue #}

import C2HS
import Data.Maybe (catMaybes)
import Control.Monad (liftM)

import System.Glib.Flags
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GValue
import System.Glib.GValueTypes
import System.Glib.GObject (makeNewGObject, GObjectClass, toGObject, unsafeCastGObject)
import Graphics.Rendering.Cairo.Types (Cairo(..), unCairo)
import qualified Graphics.Rendering.Cairo.Types as Cairo
import qualified System.Glib.GTypeConstants as GType

import Graphics.Rendering.Pango.Types (PangoContext, mkPangoContext, unPangoLayoutRaw)

--flag functions from gtk2hs with c int conversion

cToFlags :: (Flags a) => CInt ->  [a]
cToFlags = toFlags . cIntConv

cFromFlags :: (Flags a) => [a] -> CInt
cFromFlags = cIntConv . fromFlags

withCairoPath = castPtr . Cairo.unPath

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

peekNFreeMaybeString :: Ptr CChar -> IO (Maybe String)
peekNFreeMaybeString = maybeNullNew peekNFreeString

maybeNewActor :: Ptr Actor -> IO (Maybe Actor)
maybeNewActor = maybeNullNew newActor

maybeNewStage :: Ptr Actor -> IO (Maybe Stage)
maybeNewStage = maybeNullNew newStage

maybeNewAlpha :: Ptr Alpha -> IO (Maybe Alpha)
maybeNewAlpha = maybeNullNew newAlpha

--maybeNewAnimation :: Ptr Animation -> IO (Maybe Animation)
--maybeNewAnimation = maybeNullNew newAnimation

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

withPangoLayoutRaw = withForeignPtr . unPangoLayoutRaw

newPangoContext :: Ptr PangoContext -> IO PangoContext
newPangoContext p = makeNewGObject mkPangoContext (return p)

--FIXME/WTF: this should be this, but for some reason in this file
--it's getting the old version of makeNewGObject
--newPangoContext p = makeNewGObject (mkPangoContext, objectUnref) (return p)


-- gtk2hs uses Int for everything, which doesn't work for opacity at all.
clutterNewAttrFromUIntProperty :: GObjectClass gobj => String -> Attr gobj Word
clutterNewAttrFromUIntProperty propName =
  newNamedAttr propName (clutterObjectGetPropertyUInt propName) (clutterObjectSetPropertyUInt propName)

clutterObjectGetPropertyUInt :: GObjectClass gobj => String -> gobj -> IO Word
clutterObjectGetPropertyUInt = objectGetPropertyInternal GType.uint valueGetUInt

clutterObjectSetPropertyUInt :: GObjectClass gobj => String -> gobj -> Word -> IO ()
clutterObjectSetPropertyUInt = objectSetPropertyInternal GType.uint valueSetUInt


unsafeCastActor :: (ActorClass a, ActorClass b) => a -> b
unsafeCastActor = unsafeCastGObject . toGObject

#if CLUTTER_CHECK_VERSION(1,2,0)

unsafeCastLayoutManager :: (LayoutManagerClass a, LayoutManagerClass b) => a -> b
unsafeCastLayoutManager = unsafeCastGObject . toGObject

#endif

gvPtrToRealValue :: (GenericValueClass a) => GenericValuePtr -> IO a
gvPtrToRealValue = liftM unsafeExtractGenericValue . valueGetGenericValue . GValue . castPtr

