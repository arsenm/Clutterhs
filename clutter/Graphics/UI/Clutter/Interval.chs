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

-- | Value intervals — An object holding an interval of two values
module Graphics.UI.Clutter.Interval (
-- * Description
-- | 'Interval' is a simple object that can hold two values defining
-- an interval. 'Interval' can hold any value that can be enclosed
-- inside a GValue.
--
-- Once an 'Interval' for a specific GType has been instantiated the
-- "value-type" property cannot be changed anymore.
--
-- 'Interval' is used by 'Animation' to define the interval of values
-- that an implicit animation should tween over.
--
-- 'Interval' can be subclassed to override the validation and value computation.
--
-- 'Interval' is available since Clutter 1.0

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Interval'
-- @

-- * Types
  Interval,
  ProgressFunc,

-- * Constructors
  intervalNew,
  intervalClone,

-- * Methods
--intervalGetValueType,
  intervalSetInitialValue,
  intervalGetInitialValue,

  intervalSetFinalValue,
  intervalGetFinalValue,

  intervalComputeValue,
--intervalValidate,
  intervalRegisterProgressFunc

-- * Attributes
--intervalValueType
) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.StoreValue #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import Prelude
import qualified Prelude as P

import System.Glib.GValue
import System.Glib.GValueTypes
import System.Glib.Types
import System.Glib.GType



-- | Creates a new 'Interval' of type gtype, between initial and
-- final.
--
-- [@initial@] the initial value of the interval
--
-- [@final@] the final value of the interval
--
-- [@Returns@] the newly created 'Interval'
--
-- * Since 1.0
--
intervalNew :: (GenericValueClass a) => a -> a -> IO (Interval a)
intervalNew initial final = let func = {# call unsafe interval_new_with_values #}
                            in withGenericValue initial $ \iniPtr ->
                                 withGenericValue final $ \finPtr -> do
                                   gtype <- genericValuePtrGetType iniPtr   -- a little silly to gtype this way, also cast
                                   interval <- newIntervalRaw =<< func gtype iniPtr finPtr
                                   return (mkInterval (undefined :: a) interval)



-- | Creates a copy of interval.
--
-- [@interval@] an 'Interval'
--
-- [@Returns@] the newly created 'Interval'
--
-- * Since 1.0
--
intervalClone :: Interval a -> IO (Interval a)
intervalClone interval = withInterval interval $ \intervalPtr ->
                           liftM (mkInterval (undefined :: a))
                                 (newIntervalRaw =<< {# call unsafe interval_clone #} intervalPtr)




-- | Sets the initial value of interval to value.
--
-- [@interval@] an 'Interval'
--
-- [@value@] a value
--
-- * Since 1.0
--
intervalSetInitialValue :: (GenericValueClass a) => Interval a -> a -> IO ()
intervalSetInitialValue interval val = withInterval interval $ \intervalPtr ->
                                         withGenericValue val $ \valPtr ->
                                          {# call unsafe interval_set_initial_value #} intervalPtr valPtr



-- | Sets the final value of interval to value.
--
-- [@interval@] an 'Interval'
--
-- [@value@] a value
--
-- * Since 1.0
--
intervalSetFinalValue :: (GenericValueClass a) => Interval a -> a -> IO ()
intervalSetFinalValue interval val = withInterval interval $ \intervalPtr ->
                                         withGenericValue val $ \valPtr ->
                                          {# call unsafe interval_set_initial_value #} intervalPtr valPtr


--TODO: Cleanup


-- | Retrieves the initial value of interval and copies it into value.
--
-- [@interval@] an 'Interval'
--
-- [@Returns@] a value
--
-- * Since 1.0
--
intervalGetInitialValue :: (GenericValueClass a) => Interval a -> IO a
intervalGetInitialValue interval = withInterval interval $ \intervalPtr -> do
                                     gtype <- liftM cToEnum $ {# call unsafe interval_get_value_type #} intervalPtr
                                     (generic, _) <- allocaTypedGValue gtype $ \gvPtr ->
                                                       {# call unsafe interval_get_initial_value #} intervalPtr gvPtr
                                     return (unsafeExtractGenericValue generic)



-- | Retrieves the final value of interval and copies it into value.
--
-- [@interval@] an 'Interval'
--
-- [@value@] a value
--
-- * Since 1.0
--
intervalGetFinalValue :: (GenericValueClass a) => Interval a -> IO a
intervalGetFinalValue interval = withInterval interval $ \intervalPtr -> do
                                     gtype <- liftM cToEnum $ {# call unsafe interval_get_value_type #} intervalPtr
                                     (generic, _) <- allocaTypedGValue gtype $ \gvPtr ->
                                                       {# call unsafe interval_get_final_value #} intervalPtr gvPtr
                                     return (unsafeExtractGenericValue generic)




-- | Computes the value between the interval boundaries given the
-- progress factor and puts it into value.
--
-- [@interval@]  an 'Interval'
--
-- [@factor@] the progress factor, between 0 and 1
--
-- [@Returns@] @Just@ the computed value if the operation was
-- successful
--
-- * Since 1.0
--
intervalComputeValue :: (GenericValueClass a) => Interval a -> Double -> IO (Maybe a)
intervalComputeValue interval factor = let func = {# call unsafe interval_compute_value #}
                                       in withInterval interval $ \intervalPtr -> do
                                           gtype <- liftM cToEnum $ {# call unsafe interval_get_value_type #} intervalPtr
                                           (generic, ret) <- allocaTypedGValue gtype $ \gvPtr -> do
                                                               liftM cToBool (func intervalPtr (cFloatConv factor) gvPtr)
                                           return $ if ret
                                                      then Just (unsafeExtractGenericValue generic)
                                                      else P.Nothing


type ProgressFunc a = a -> a -> Double -> (Bool, a)
type CProgressFunc = FunPtr (GenericValuePtr -> GenericValuePtr -> CDouble -> GenericValuePtr -> IO {# type gboolean #})

-- CHECKME: Never free the function
-- CHECKME: I think you will need to specify the gtype to avoid possibly breaking things
intervalRegisterProgressFunc :: (GenericValueClass a) => GType -> ProgressFunc a -> IO ()
intervalRegisterProgressFunc gtype pf = let func = {# call clutter_interval_register_progress_func #}
                                        in newProgressFunc pf >>= func gtype


newProgressFunc :: (GenericValueClass a) => ProgressFunc a -> IO CProgressFunc
newProgressFunc userfunc = mkProgressFunc (newProgressFunc' userfunc)
    where
      newProgressFunc' :: (GenericValueClass a) => ProgressFunc a -> GenericValuePtr -> GenericValuePtr -> Double -> GenericValuePtr -> IO Bool
      newProgressFunc' userfunc aPtr bPtr p retPtr = do a <- gvPtrToRealValue aPtr
                                                        b <- gvPtrToRealValue bPtr
                                                        let (stat, retVal) = userfunc a b p
                                                            retGV = GValue (castPtr retPtr)
                                                        valueSetGenericValueNoInit retGV (toGenericValue retVal)
                                                        return stat
foreign import ccall "wrapper"
    mkProgressFunc :: (GenericValuePtr -> GenericValuePtr -> Double -> GenericValuePtr -> IO Bool) -> IO CProgressFunc

