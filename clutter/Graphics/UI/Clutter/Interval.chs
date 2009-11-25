-- -*-haskell-*-
--  Clutter Interval
--
--  Author : Matthew Arsenault
--
--  Created: 2 Oct 2009
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
{-# LANGUAGE ForeignFunctionInterface,
             ScopedTypeVariables       #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Interval (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Interval'
-- @

-- * Constructors
  intervalNew,
  intervalClone,

-- * Methods

--intervalGetValueType,
  intervalSetInitialValue,
  intervalGetInitialValue,

--intervalPeekInitalValue,
  intervalSetFinalValue,
  intervalGetFinalValue,

--intervalPeekFinalValue,
--intervalSetInterval,
--intervalGetInterval,

  intervalComputeValue,
--intervalValidate,
--intervalRegisterProgressFunc

-- * Attributes
--intervalInitialValue,
--intervalFinalValue,
--intervalInterval,
) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.StoreValue #}

import C2HS
import Control.Monad (liftM)
import Prelude
import qualified Prelude as P

import System.Glib.GValue

intervalNew :: (GenericValueClass a) => a -> a -> IO (Interval a)
intervalNew initial final = let func = {# call unsafe interval_new_with_values #}
                            in withGenericValue initial $ \iniPtr ->
                                 withGenericValue final $ \finPtr -> do
                                   gtype <- genericValuePtrGetType iniPtr   -- a little silly to gtype this way, also cast
                                   interval <- newIntervalRaw =<< func gtype iniPtr finPtr
                                   return (mkInterval (undefined :: a) interval)


intervalClone :: Interval a -> IO (Interval a)
intervalClone interval = withInterval interval $ \intervalPtr ->
                           liftM (mkInterval (undefined :: a))
                                 (newIntervalRaw =<< {# call unsafe interval_clone #} intervalPtr)

intervalSetInitialValue :: (GenericValueClass a) => Interval a -> a -> IO ()
intervalSetInitialValue interval val = withInterval interval $ \intervalPtr ->
                                         withGenericValue val $ \valPtr ->
                                          {# call unsafe interval_set_initial_value #} intervalPtr valPtr

intervalSetFinalValue :: (GenericValueClass a) => Interval a -> a -> IO ()
intervalSetFinalValue interval val = withInterval interval $ \intervalPtr ->
                                         withGenericValue val $ \valPtr ->
                                          {# call unsafe interval_set_initial_value #} intervalPtr valPtr


--TODO: Cleanup
intervalGetInitialValue :: (GenericValueClass a) => Interval a -> IO a
intervalGetInitialValue interval = withInterval interval $ \intervalPtr -> do
                                     gtype <- liftM cToEnum $ {# call unsafe interval_get_value_type #} intervalPtr
                                     generic <- allocaTypedGValue gtype $ \gvPtr ->
                                                       {# call unsafe interval_get_initial_value #} intervalPtr gvPtr
                                     return (unsafeExtractGenericValue generic)

intervalGetFinalValue :: (GenericValueClass a) => Interval a -> IO a
intervalGetFinalValue interval = withInterval interval $ \intervalPtr -> do
                                     gtype <- liftM cToEnum $ {# call unsafe interval_get_value_type #} intervalPtr
                                     generic <- allocaTypedGValue gtype $ \gvPtr ->
                                                       {# call unsafe interval_get_final_value #} intervalPtr gvPtr
                                     return (unsafeExtractGenericValue generic)




intervalComputeValue :: (GenericValueClass a) => Interval a -> Double -> IO (Maybe a)
intervalComputeValue interval factor = let func = {# call unsafe interval_compute_value #}
                                       in withInterval interval $ \intervalPtr ->
                                            allocaGValue $ \gv@(GValue gvPtr) -> do
                                              ret <- liftM cToBool $ func intervalPtr (cFloatConv factor) (castPtr gvPtr)
                                              generic <- valueGetGenericValue gv
                                              return $ if ret
                                                         then Just (unsafeExtractGenericValue generic)
                                                         else P.Nothing


--intervalRegisterProgressFunc :: (GenericValueClass a) => GType -> a -> a -> Double -> IO (Maybe a)

