-- -*-haskell-*-
--  GValue
--
--  Author : Matthew Arsenault
--
--  Created: 25 Sep 2009
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
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.GValue (
                                   withGValue,
                                   animateTest,
                                  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS

import System.Glib.GObject
import System.Glib.GType
import System.Glib.GValue

import qualified System.Glib.GTypeConstants as GType

import Control.Monad (liftM)


--quick thing to get compiling.
type Alpha = Int

--this seems like it should have been done already.  The motivation is
--you don't need to do an explicit conversion / creation of a GValue
--when you try to use clutter_animatev and co
class GValueClass a where
  toGValue :: a -> IO GValue
  withGValue :: a -> (GValue -> IO b) -> IO b
--  withManyGValues :: [a] -> ([GValue] -> IO b) -> IO b
--toGValue :: (GValueClass self) => self -> GValue


--this is an unholy idea.
--really I only want the convenience of withArrayLen
instance Storable GValue where
  --according to allocaGValue in gtk2hs, this is the size they use
  sizeOf _ = {# sizeof GValue #}
  alignment _ = alignment (undefined :: GType)

--withGValues gvs body = withArrayLen $ \n ptr ->


--FIXME: Int vs. GUInt again.
--FIXME: This is an issue...Using a list for this,
--every member of the list must be of the same type.
--I don't think this can be fixed with classes.
--Hmmmmmmmmmmmmmmm...may still need stupid conversion.
{-
withValArrays :: (GValueClass a) => [(String,a)]
              -> (Int -> Ptr String -> Ptr a -> IO b)
              -> IO b
-}
--do it with gtype first. maybe a better solution later
{-
withValArrays :: [(String, GValue)]
              -> (Int -> Ptr String -> Ptr GValue -> IO b)
              -> IO b
withValArrays vals f  =
  allocaArray len $ \ptr -> do

      res <- f len ptr
      return res
  where
    len = length vals
-}

--since want ahead of time toGvalue'ing
--try this finalizer stuff
{-
foreign import ccall "glib.h &g_value_unset"
   valueUnset :: FunPtr (Ptr a -> IO ())

instance GValueClass Int where
  withGValue val body = allocaGValue $ \gv ->
                         valueInit gv GType.int >>
                         body gv
  toGValue val = do
  --so sayeth the gtk2hs
      gvPtr <- mallocBytes ({# sizeof GType #} + 2*{# sizeof guint64 #})
      {# set GValue->g_type #} gvPtr (0 :: GType)
      fptr <- newForeignPtr finalizerFree gvPtr
      addForeignPtrFinalizer valueUnset fptr
      return fptr

instance GValueClass Bool where
  withGValue val body = allocaGValue $ \gv ->
                        valueInit gv GType.bool >>
                        body gv

instance GValueClass String where
  withGValue val body = allocaGValue $ \gv ->
                        valueInit gv GType.string >>
                        body gv

-}


--I think now that the best solution is variable number of arguments,
--where each extra argument is a pair.  not sure how to do this, or if
--it will work. It will solve the different types issue. I
--think. maybe.  it works in any case for printf. Then somehow pack
--into arrays, get n and use animatev

animateTest :: (ActorClass actor, AnimateType r) => actor -> Alpha -> r
animateTest = animate

animate :: (ActorClass actor, AnimateType r) => actor -> Alpha -> r
animate actor alpha = spr actor alpha []

class AnimateType t where
    spr :: (ActorClass actor) => actor -> Alpha -> [UAnimate] -> t

{- Multiple return types isn't useful. We only want IO Animation
instance (Integral n) => AnimateType [n] where
    spr actor alpha args = error ("LOLOL: " ++ show args)
--    spr actor alpha args = map fromChar (uprintf fmts (reverse args))
-}
-- this is how Text.printf does variable number of arguments
instance AnimateType (IO a) where
    spr actor alpha args =
        uanimate actor alpha args >> return undefined
    --	putStrLn (show (reverse args))

instance (AnimateArg a, AnimateType r) => AnimateType (a -> r) where
    spr actor alpha args = \ a -> spr actor alpha (toUAnimate a : args)


class AnimateArg a where
    toUAnimate :: a -> UAnimate

instance (IsChar c) => AnimateArg [c] where
    toUAnimate = UString . map toChar

instance AnimateArg Int where
    toUAnimate = uInteger

instance AnimateArg Double where
    toUAnimate = UDouble

uInteger :: (Integral a, Bounded a) => a -> UAnimate
uInteger x = UInteger (fromIntegral x)

class IsChar c where
    toChar :: c -> Char
    fromChar :: Char -> c

instance IsChar Char where
    toChar c = c
    fromChar c = c

--by UAnimate, I mean GValue that animatev accepts. Fix that later.
--Not sure this is how I want to do this.

data UAnimate = UChar Char
              | UString String
              | UInteger Int
              | UFloat Float
              | UDouble Double
              deriving (Show, Eq)

--FIXME: This error somehow isn't happening
uanimate :: (ActorClass actor) => actor -> Alpha -> [UAnimate] -> IO ()
unimate _ _ [] = error "Need arguments to animate?"
uanimate actor alpha us = putStrLn (show us)

