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

module Graphics.UI.Clutter.AnimateArgs (
                                        animate,
                                        uanimate
                                       ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Animation #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS

import System.Glib.GObject
import System.Glib.GType
import System.Glib.GValue
import System.Glib.Attributes
import System.Glib.Properties

import System.Glib.GValueTypes
import Control.Arrow (second)
import Control.Monad (foldM_)

import qualified System.Glib.GTypeConstants as GType

import Control.Monad (liftM, foldM)

--FIXME: Types as usual
--by UAnimate, I mean GValue that animatev accepts. Fix that later.
--Not sure this is how I want to do this.
data UAnimate = UChar Char
              | UString String
              | UUChar Word8
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

--TODO: Type for Duration, type Duration = UInt or whatever

animate :: (ActorClass actor, AnimateType r) => actor -> AnimationMode -> Int -> r
animate actor mode duration = spr actor mode duration []

class AnimateType t where
    spr :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, UAnimate)] -> t

{- Multiple return types isn't useful. We only want IO Animation
instance (Integral n) => AnimateType [n] where
    spr actor alpha args = error ("LOLOL: " ++ show args)
--    spr actor alpha args = map fromChar (uprintf fmts (reverse args))
-}
-- this is how Text.printf does variable number of arguments
instance AnimateType (IO Animation) where
    spr actor mode duration args = uanimate2 actor mode duration args

instance (AnimateArg a, AnimateType r) => AnimateType (a -> r) where
    spr actor mode duration args = \ a -> spr actor mode duration (toUAnimate a : args)


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

instance AnimateArg (String, Float) where
    toUAnimate = second UFloat

instance AnimateArg (String, Double) where
    toUAnimate = second UDouble

instance AnimateArg (String, Word8) where
    toUAnimate = second UUChar

uInteger :: (Integral a, Bounded a) => a -> UAnimate
uInteger x = UInteger (fromIntegral x)

class IsChar c where
    toChar :: c -> Char
    fromChar :: Char -> c

instance IsChar Char where
    toChar c = c
    fromChar c = c

--TODO: Maybe redo this differently
-- one option is to map through the list binding each property individually
-- alternatively, pack into array of gvalue and use actor_animatev function
--Also can probably shorten / simplify the UAnimate type garbage
uanimate :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, UAnimate)] -> IO Animation
uanimate _ _ _ [] = error "Need arguments to animate?"
uanimate actor mode duration us = do
  anim <- animationNew
  set anim [animationMode := mode,
            animationDuration := duration,
            animationObject := toGObject actor]
  foldM bindOne anim us
    where bindOne anim (name, UInteger val) = allocaGValue $ \gVal ->
                                              valueInit gVal GType.int >>
                                              valueSetInt gVal val >>
                                              animationBind anim name gVal
          bindOne anim (name, UString val) = allocaGValue $ \gVal ->
                                             valueInit gVal GType.string >>
                                             valueSetString gVal val >>
                                             animationBind anim name gVal
          bindOne anim (name, UFloat val) = allocaGValue $ \gVal ->
                                             valueInit gVal GType.float >>
                                             valueSetFloat gVal val >>
                                             animationBind anim name gVal
          bindOne anim (name, UDouble val) = allocaGValue $ \gVal ->
                                             valueInit gVal GType.double >>
                                             valueSetDouble gVal val >>
                                             animationBind anim name gVal
--FIXME/TODO/WTF: valueSetChar/valueSetUChar is commented out in gtk2hs...
--See if it works with int
          bindOne anim (name, UUChar val) = allocaGValue $ \gVal ->
                                             valueInit gVal GType.int >>
                                             valueSetInt gVal (fromIntegral val) >>
                                             animationBind anim name gVal
--It will be miraculous if this actually works
uanimate2 :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, UAnimate)] -> IO Animation
uanimate2 _ _ _ [] = error "Need arguments to animate?"
uanimate2 actor mode duration us =
    let (names, uvals) = unzip us
        size = {# sizeof GValue #}
        l = length us
        n = l * size
        --FIXME: unsafe?
        animatev = {# call unsafe actor_animatev #}
    in do
    cstrs <- mapM newCString names
    res <- withArrayLen cstrs $ \len strptr ->
           withActorClass actor $ \actptr ->
           allocaBytes n $ \gvPtr ->
            let unsetOne m u = {#call unsafe g_value_unset#} (plusPtr gvPtr (m*n)) >> return (m+1)
                setOne m uval = {# set GValue->g_type #} i (0 :: GType) >>
                                plugValue (GValue i) uval >>
                                return (m+1)
                                    where
                                      i = plusPtr gvPtr (m*n)
                plugValue gVal (UInteger val) = valueInit gVal GType.int >> valueSetInt gVal val
                plugValue gVal (UFloat val) = valueInit gVal GType.float >> valueSetFloat gVal val
                plugValue gVal (UDouble val) = valueInit gVal GType.double >> valueSetDouble gVal val
                plugValue gVal (UString val) = valueInit gVal GType.string >> valueSetString gVal val
                plugValue _ uval = error $ "Type needs to be done for uanimate" ++ show uval
            in do
              foldM_ setOne 0 uvals
              result <- animatev actptr (cFromEnum mode) (cIntConv duration) (cIntConv len) strptr gvPtr
              foldM_ unsetOne 0 uvals
              return result
              --FIXME: UInt vs. Int yet again. I should really just fix it already everywhere
    mapM free cstrs
    newres <- newAnimation res
    return newres


