{-# LANGUAGE FlexibleInstances #-}
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
import System.Glib.GValueTypes
import Control.Arrow (second)

import qualified System.Glib.GTypeConstants as GType

import Control.Monad (liftM)



{-

withGValue2 :: (GValueClass a) => a -> (Ptr GValue -> IO b) -> IO b
withGValue2 val body =
  -- c2hs is broken in that it can't handle arrays of compound arrays in the
  -- sizeof hook
  allocaBytes ({# sizeof GType #}+ 2* {# sizeof guint64 #}) $ \gvPtr -> do
  -- The g_type field of the value must be zero or g_value_init will fail.
  {# set GValue->g_type #} gvPtr (0 :: GType)
  result <- body (GValue gvPtr)
  {#call unsafe value_unset#} (GValue gvPtr)
  return result
-}

--withGValue3 (GValue fptr) = withForeignPtr fptr

--this seems like it should have been done already.  The motivation is
--you don't need to do an explicit conversion / creation of a GValue
--when you try to use clutter_animatev and co
class GValueClass a where
--  toGValue :: a -> IO GValue
--  withGValue :: a -> (GValue -> IO b) -> IO b
  withGValue :: a -> (GValue -> IO b) -> IO b

--by UAnimate, I mean GValue that animatev accepts. Fix that later.
--Not sure this is how I want to do this.

data UAnimate = UChar Char
              | UString String
              | UInteger Int
              | UFloat Float
              | UDouble Double
              deriving (Show, Eq)

--there's probably a nicer/shorter way to do this
instance GValueClass UAnimate where
  withGValue (UInteger a) body = allocaGValue $ \gv ->
                                 valueInit gv GType.int >>
                                 valueSetInt gv a >>
                                 body gv
  withGValue (UDouble a) body = allocaGValue $ \gv ->
                                valueInit gv GType.double >>
                                valueSetDouble gv a >>
                                body gv
  withGValue (UString a) body = allocaGValue $ \gv ->
                                valueInit gv GType.string >>
                                valueSetString gv a >>
                                body gv

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

animateTest :: (AnimateType r) => Animation -> Alpha -> r
animateTest = animate

animate :: (AnimateType r) => Animation -> Alpha -> r
animate actor alpha = spr actor alpha []

class AnimateType t where
    spr :: Animation -> Alpha -> [(String, UAnimate)] -> t

{- Multiple return types isn't useful. We only want IO Animation
instance (Integral n) => AnimateType [n] where
    spr actor alpha args = error ("LOLOL: " ++ show args)
--    spr actor alpha args = map fromChar (uprintf fmts (reverse args))
-}
-- this is how Text.printf does variable number of arguments
instance AnimateType (IO a) where
    spr actor alpha args =
        uanimate actor alpha args >> return undefined

instance (AnimateArg a, AnimateType r) => AnimateType (a -> r) where
    spr actor alpha args = \ a -> spr actor alpha (toUAnimate a : args)


--this should always be a pair of a name and something which can be a gvalue
-- (String, Something that can be a GValue)
--how to enforce this nicely? Is this good enough?
class AnimateArg a where
    toUAnimate :: a -> (String, UAnimate)

--Making these pairs instances of the arg class
--requires FlexibleInstances extension
--Is there any good way around this?
instance (IsChar c) => AnimateArg (String, [c]) where
    toUAnimate = second (UString . map toChar)

instance AnimateArg (String, Int) where
    toUAnimate = second uInteger

instance AnimateArg (String, Double) where
    toUAnimate = second UDouble

uInteger :: (Integral a, Bounded a) => a -> UAnimate
uInteger x = UInteger (fromIntegral x)

class IsChar c where
    toChar :: c -> Char
    fromChar :: Char -> c

instance IsChar Char where
    toChar c = c
    fromChar c = c


--FIXME: This error somehow isn't happening
uanimate :: Animation -> Alpha -> [(String, UAnimate)] -> IO ()
unimate _ _ [] = error "Need arguments to animate?"
uanimate anim alpha us = putStrLn (show us)
{-
uanimate anim alpha us = mapM_ bindOne us
    where bindOne (a,b) = do
    with
    animationBind anim a (uAnimateToGValue)
-}

-- one option is to map through the list binding each property individually
-- alternatively, pack into array of gvalue and use actor_animatev function

--This shouldn't be this messy. Why do I need the casting? And use fun
--I'm still missing something about the wrapping stuff
--Peter says that won't work
animationBind:: Animation -> String -> Int -> IO Animation
animationBind self name val = do
  b <- allocaGValue $ \gVal ->
       withCString name $ \str ->
       withAnimation self $ \anptr ->
       valueInit gVal GType.int >>
       valueSetInt gVal val >>
      {# call animation_bind #} anptr str (unGValue gVal)
  newAnimation b

--I bet this won't work.
unGValue :: GValue -> Ptr ()
unGValue (GValue a) = castPtr a


{-
{# pointer *GValue newtype #}
newtype GValue = GValue (Ptr (GValue))
-}
