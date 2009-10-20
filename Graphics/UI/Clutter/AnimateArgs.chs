-- -*-haskell-*-
--  AnimateArgs
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
{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}

#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.AnimateArgs (
                                        animate,
                                        animateWithAlpha,
                                        uanimate,
                                        uanimatewithalpha
                                       ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Animation #}
{# import Graphics.UI.Clutter.GValue #}
--{# import Graphics.UI.Clutter.External #}

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
--by UAnimate, I mean GValue that animatev accepts. Fix that later (rename)
--Not sure this is how I want to do this.
--FIXME: the gvalue set/get functions for char are commented out in gtk2hs...
--It's probably a better idea to have a GValueClass, and then an init, set and get function
data UAnimate = UChar Char
              | UString String
              | UUChar Word8
              | UInteger Int
              | UFloat Float
              | UDouble Double
              | UColor Color
              | UGObject GObject

{# pointer *GValue as UanimatePtr -> UAnimate #}
--FIXME: Rename this and fix the need for cast. i.e. get rid of uanimate type sort of.
instance Storable UAnimate where
    sizeOf _ = {# sizeof GValue #}
    alignment _ = alignment (undefined :: GType)
    peek _ = error "peek undefined for GValue"
    poke p ut = let gv = GValue (castPtr p) --FIXME: This castPtr = badnews bears?
                in do
                {# set GValue->g_type #} p (0 :: GType)
                case ut of
                --FIXME: Char/UCHar vs. Int8 type sort of hacky,
                --intconv, why missing char marshaller?
                  (UInteger val) -> gValueInitSet gv val
                  (UDouble val) ->  gValueInitSet gv val
                  (UFloat val) -> gValueInitSet gv val
                  (UString val) ->  gValueInitSet gv val
                  (UChar val) ->  gValueInitSet gv val
                  (UUChar val) -> gValueInitSet gv val
                  (UColor val) -> gValueInitSet gv val
                  (UGObject val) -> gValueInitSet gv val

--TODO: Type for Duration, type Duration = UInt or whatever

animate :: (ActorClass actor, AnimateType r) => actor -> AnimationMode -> Int -> r
animate actor mode duration = runAnim actor mode duration []

animateWithAlpha :: (ActorClass actor, AnimateType r) => actor -> Alpha -> r
animateWithAlpha actor alpha = runAnimHack actor alpha []

class AnimateType t where
    runAnim :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, UAnimate)] -> t
    runAnimHack :: (ActorClass actor) => actor -> Alpha -> [(String, UAnimate)] -> t

-- Multiple return types isn't useful. We only want IO Animation.
-- This breaks things if you remove the instance for ().
-- Also type inference not working in many cases. maybe find a better way
instance AnimateType (IO Animation) where
    runAnim actor mode duration args = uanimate actor mode duration args
    runAnimHack actor alpha args = uanimatewithalpha actor alpha args

instance AnimateType (IO ()) where
    runAnim actor mode duration args = uanimate actor mode duration args >> return ()
    runAnimHack actor alpha args = uanimatewithalpha actor alpha args >> return ()

instance (AnimateArg a, AnimateType r) => AnimateType (a -> r) where
    runAnim actor mode duration args = \a -> runAnim actor mode duration (toUAnimate a : args)
    runAnimHack actor alpha args = \a -> runAnimHack actor alpha (toUAnimate a : args)


--this should always be a pair of a name and something which can be a gvalue
-- (String, Something that can be a GValue)
--how to enforce this nicely? Is this good enough?
class AnimateArg a where
    toUAnimate :: a -> (String, UAnimate)

instance AnimateArg (String, String) where
    toUAnimate = second UString
instance AnimateArg (String, Int) where
    toUAnimate = second UInteger
instance AnimateArg (String, Float) where
    toUAnimate = second UFloat
instance AnimateArg (String, Color) where
    toUAnimate = second UColor
instance AnimateArg (String, Double) where
    toUAnimate = second UDouble
instance AnimateArg (String, Word8) where
    toUAnimate = second UUChar
instance (GObjectClass obj) => AnimateArg (String, obj) where
    toUAnimate = second (UGObject . toGObject)
--how the hell does gobjectclass overlap float?

unsetOneGVal i u = {#call unsafe g_value_unset#} i >> return (advancePtr i 1)

uanimate :: (ActorClass actor) => actor -> AnimationMode -> Int -> [(String, UAnimate)] -> IO Animation
uanimate _ _ _ [] = error "Need arguments to animate"
uanimate actor mode duration us =
    let (names, uvals) = unzip us
        size = {# sizeof GValue #}
        --CHECKME: unsafe?
        animatev = {# call unsafe actor_animatev #}
    in
    withMany withCString names $ \cstrs -> do
      res <- withArrayLen cstrs $ \len strptr ->
             withActorClass actor $ \actptr ->
             withArray uvals $ \gvPtr -> do
               result <- animatev actptr (cFromEnum mode) (cIntConv duration) (cIntConv len) strptr gvPtr
               foldM_ unsetOneGVal gvPtr uvals
               return result
      --FIXME: UInt vs. Int yet again. I should really just fix it already everywhere
      newAnimation res  --FIXME: Do I need to do this here? reffing?

--FIXME: Duplication here?

uanimatewithalpha :: (ActorClass actor) => actor -> Alpha -> [(String, UAnimate)] -> IO Animation
uanimatewithalpha _ _ [] = error "Need arguments to animate with alpha"
uanimatewithalpha actor alpha us =
    let (names, uvals) = unzip us
        size = {# sizeof GValue #}
        --CHECKME: unsafe?
        animatev = {# call unsafe actor_animate_with_alphav #}
    in
    withMany withCString names $ \cstrs -> do
      res <- withArrayLen cstrs $ \len strptr ->
           withActorClass actor $ \actptr ->
           withAlpha alpha $ \alphptr ->
           withArray uvals $ \gvPtr -> do
             result <- animatev actptr alphptr (cIntConv len) strptr gvPtr
             foldM_ unsetOneGVal gvPtr uvals
             return result
              --FIXME: UInt vs. Int yet again. I should really just fix it already everywhere
      newAnimation res  --CHECKME: Do I need to do this here? reffing?

