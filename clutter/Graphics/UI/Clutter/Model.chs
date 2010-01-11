-- -*-haskell-*-
--  Clutter Model
--
--  Author : Matthew Arsenault
--
--  Created: 6 Oct 2009
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

--TODO: Make it like models in gtk2hs

module Graphics.UI.Clutter.Model (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Model'
-- |           +----'ListModel'
-- @

-- * Types
  Model,
  ModelClass,
  ModelForeachFunc,

  ColCons,
  EOC(..),
  ColList,
  (##),
  single,

-- * Methods

--modelSetNames,  -- I don't think these 2 make sense to bind
--modelSetTypes,

  modelGetColumnName,
  modelGetColumnType,
  modelGetNColumns,
  modelGetNRows,
  modelAppend,
  modelPrepend,
  modelInsert,
  modelInsertValue,
  modelRemove,
  modelForeach,
  modelSetSortingColumn,
  modelGetSortingColumn,
  modelSetSort,
  modelResort,
  modelSetFilter,
  modelGetFilterSet,
  modelFilterIter,
  modelFilterRow,
  modelGetFirstIter,
  modelGetLastIter,
  modelGetIterAtRow,
-- * Attributes
  modelFilterSet,

-- * Signals
  onFilterChanged,
  afterFilterChanged,
  filterChanged,
  onRowAdded,
  afterRowAdded,
  rowAdded,
  onRowRemoved,
  afterRowRemoved,
  rowRemoved,
  onSortChanged,
  afterSortChanged,
  sortChanged
  ) where

{# import Graphics.UI.Clutter.Types #} hiding (Nothing)
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.StoreValue #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GValue
import System.Glib.GType
import Data.Word
import Control.Monad (when, liftM)

{# fun unsafe model_get_column_name as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `String' #}

{# fun unsafe model_get_column_type as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `GType' cToEnum #}

{# fun unsafe model_get_n_columns as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Word' cIntConv #}

{# fun unsafe model_get_n_rows as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Word' cIntConv #}


modelAppend :: (ModelClass self, ColList a w) => self -> a -> [Int] -> IO ()
modelAppend model cols nums = let func = {# call model_appendv #}
                                  gvs = toHomoGVList cols
                                  l = length gvs
                              in withModelClass model $ \modelPtr ->
                                   withArray gvs $ \gvPtr ->
                                     withArrayLen nums $ \len iPtr -> do
                                       when (l /= len) (fail "modelAppend: Length of column does not match columns to update")
                                       func modelPtr (cIntConv l) (castPtr iPtr) gvPtr

modelPrepend :: (ModelClass self, ColList a w) => self -> a -> [Int] -> IO ()
modelPrepend model cols nums = let func = {# call model_prependv #}
                                   gvs = toHomoGVList cols
                                   l = length gvs
                               in withModelClass model $ \modelPtr ->
                                    withArray gvs $ \gvPtr ->
                                      withArrayLen nums $ \len iPtr -> do
                                        when (l /= len) (fail "modelPrepend: Length of column does not match columns to update")
                                        func modelPtr (cIntConv l) (castPtr iPtr) gvPtr




modelInsert :: (ModelClass self, ColList a w) => self -> Word -> a -> [Int] -> IO ()
modelInsert model row cols nums = let func = {# call model_insertv #}
                                      gvs = toHomoGVList cols
                                      l = length gvs
                                  in withModelClass model $ \modelPtr ->
                                       withArray gvs $ \gvPtr ->
                                         withArrayLen nums $ \len iPtr -> do
                                           when (l /= len) (fail "modelInsert: Length of column does not match columns to update")
                                           func modelPtr (cIntConv row) (cIntConv len) (castPtr iPtr) gvPtr

--FIXME: No checking for correct type
modelInsertValue :: (ModelClass self, GenericValueClass val) => self -> Word -> Word -> val -> IO ()
modelInsertValue model row col val = let func = {# call unsafe model_insert_value #}
                                     in withModelClass model $ \modelPtr ->
                                          withGenericValue val $ \valPtr ->
                                            func modelPtr (cIntConv row) (cIntConv col) valPtr


{# fun unsafe model_get_sorting_column as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Int' #}

{# fun unsafe model_set_sorting_column as ^
       `(ModelClass model)' => { withModelClass* `model', `Int' } -> `()' #}

nullFptr :: FunPtr a
nullFptr = castPtrToFunPtr nullPtr

--FIXME: Type checking...
modelSetSort :: (ModelClass model, GenericValueClass a) => model -> Word -> Maybe (ModelSortFunc model a) -> IO ()
modelSetSort model col userfunc = let func = {# call model_set_sort #}
                                  in withModelClass model $ \mPtr -> do
                                    case userfunc of
                                      Nothing -> func mPtr (cIntConv col) nullFptr nullPtr nullFptr
                                      Just x -> do
                                        fPtr <- newModelSortFunc x
                                        func mPtr (cIntConv col) fPtr (castFunPtrToPtr fPtr) destroyFunPtr



{# fun unsafe model_remove as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `()' #}


modelForeach :: (ModelClass b) => b -> ModelForeachFunc -> IO ()
modelForeach b func = withModelClass b $ \mptr -> do
                        funcPtr <- newModelForeachFunc func
                        {# call model_foreach #} mptr funcPtr nullPtr
                        freeHaskellFunPtr funcPtr

{# fun unsafe model_resort as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `()' #}


modelSetFilter :: (ModelClass model) => model -> Maybe ModelFilterFunc -> IO ()
modelSetFilter model filterFunc = let func = {# call model_set_filter #}
                                  in withModelClass model $ \mdlPtr -> do
                                    case filterFunc of
                                      Nothing -> func mdlPtr nullFptr nullPtr nullFptr
                                      Just x -> do
                                        fFuncPtr <- newModelFilterFunc x
                                        func mdlPtr fFuncPtr (castFunPtrToPtr fFuncPtr) destroyFunPtr


{# fun unsafe model_get_filter_set as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Bool' #}

{# fun unsafe model_filter_iter as ^
       `(ModelClass model)' => { withModelClass* `model', withModelIter* `ModelIter' } -> `Bool' #}

{# fun unsafe model_filter_row as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `Bool' #}

{# fun unsafe model_get_first_iter as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `ModelIter' newModelIter* #}
{# fun unsafe model_get_last_iter as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `ModelIter' newModelIter* #}
{# fun unsafe model_get_iter_at_row as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `ModelIter' newModelIter* #}


-- Attributes

modelFilterSet :: (ModelClass self) => ReadAttr self Bool
modelFilterSet = readAttrFromBoolProperty "filter-set"


-- Signals

onFilterChanged, afterFilterChanged :: Model -> IO () -> IO (ConnectId Model)
onFilterChanged = connect_NONE__NONE "filter-changed" False
afterFilterChanged = connect_NONE__NONE "filter-changed" True

filterChanged :: Signal Model (IO ())
filterChanged = Signal (connect_NONE__NONE "filter-changed")


onRowAdded, afterRowAdded :: Model -> (ModelIter -> IO ()) -> IO (ConnectId Model)
onRowAdded = connect_OBJECT__NONE "row-added" False
afterRowAdded = connect_OBJECT__NONE "row-added" True

rowAdded :: Signal Model (ModelIter ->IO ())
rowAdded = Signal (connect_OBJECT__NONE "row-added")


onRowChanged, afterRowChanged :: Model -> (ModelIter -> IO ()) -> IO (ConnectId Model)
onRowChanged = connect_OBJECT__NONE "row-changed" False
afterRowChanged = connect_OBJECT__NONE "row-changed" True

rowChanged :: Signal Model (ModelIter ->IO ())
rowChanged = Signal (connect_OBJECT__NONE "row-changed")


onRowRemoved, afterRowRemoved :: Model -> (ModelIter -> IO ()) -> IO (ConnectId Model)
onRowRemoved = connect_OBJECT__NONE "row-removed" False
afterRowRemoved = connect_OBJECT__NONE "row-removed" True

rowRemoved :: Signal Model (ModelIter -> IO ())
rowRemoved = Signal (connect_OBJECT__NONE "row-removed")


onSortChanged, afterSortChanged :: Model -> IO () -> IO (ConnectId Model)
onSortChanged = connect_NONE__NONE "sort-changed" False
afterSortChanged = connect_NONE__NONE "sort-changed" True

sortChanged :: Signal Model (ModelIter -> IO ())
sortChanged = Signal (connect_OBJECT__NONE "sort-changed")


-- | end of heterogenous list of types for a column
data EOC = EOC deriving (Read, Show)

-- | heterogenous list of types for a column
data ColCons a b = ColCons a b deriving (Read, Show)

-- it looks kind of like columns or something
infixr 0 ##
-- | Prepend an element to a heterogeneous list. Used to build columns
(##) :: (GenericValueClass a, ColList b w) => a -> b -> ColCons a b
(##) = ColCons

-- | Construct a singleton list of columns
single :: (GenericValueClass a) => a -> ColCons a EOC
single = (## EOC)

class ColList c d | c -> d where
  toHomoGVList :: c -> [GenericValue]

instance ColList EOC w where
  toHomoGVList EOC = []

instance (GenericValueClass a, ColList b w) => ColList (ColCons a b) w where
  toHomoGVList (ColCons x xs) = toGenericValue x:(toHomoGVList xs)


-- *** ModelSortFunc

type ModelSortFunc m a = m -> a -> a -> IO Int
type CModelSortFunc = FunPtr (Ptr Model -> GenericValuePtr -> GenericValuePtr -> Ptr () -> IO CInt)

newModelSortFunc :: (ModelClass m, GenericValueClass a) => ModelSortFunc m a -> IO CModelSortFunc
newModelSortFunc userfunc = mkModelSortFunc (newModelSortFunc' userfunc)
    where
      newModelSortFunc' :: (GenericValueClass a, ModelClass model) => (model -> a -> a -> IO Int) -> Ptr Model -> GenericValuePtr -> GenericValuePtr -> IO CInt
      newModelSortFunc' userfunc mPtr aPtr bPtr = do a <- gvPtrToRealValue aPtr
                                                     b <- gvPtrToRealValue bPtr
                                                     model <- liftM (unsafeCastGObject . toGObject) (newModel mPtr)
                                                     liftM cIntConv (userfunc model a b)

foreign import ccall "wrapper"
    mkModelSortFunc :: (Ptr Model -> GenericValuePtr -> GenericValuePtr -> IO CInt) -> IO CModelSortFunc

