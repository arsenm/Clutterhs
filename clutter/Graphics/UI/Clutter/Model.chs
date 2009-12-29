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

-- * Methods

--modelSetNames,  -- I don't think these 2 make sense to bind
--modelSetTypes,

  modelGetColumnName,
  modelGetColumnType,
  modelGetNColumns,
  modelGetNRows,
--modelAppend,
--modelPrepend,
--modelInsert,
--modelInsertValue,
  modelRemove,
  modelForeach,
  modelSetSortingColumn,
  modelGetSortingColumn,
--modelSetSort,
  modelResort,
--modelSetFilter,
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

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GType
import Data.Word

{# fun unsafe model_get_column_name as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `String' #}

{# fun unsafe model_get_column_type as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `GType' cToEnum #}

{# fun unsafe model_get_n_columns as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Word' cIntConv #}

{# fun unsafe model_get_n_rows as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Word' cIntConv #}


{# fun unsafe model_get_sorting_column as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `Word' cIntConv #}
{# fun unsafe model_set_sorting_column as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `()' #}

{# fun unsafe remove as ^
       `(ModelClass model)' => { withModelClass* `model', cIntConv `Word' } -> `()' #}


modelForeach :: (ModelClass b) => b -> ModelForeachFunc -> IO ()
modelForeach b func = withModelClass b $ \mptr -> do
                        funcPtr <- newModelForeachFunc func
                        {# call model_foreach #} mptr funcPtr nullPtr
                        freeHaskellFunPtr funcPtr

{# fun unsafe model_resort as ^
       `(ModelClass model)' => { withModelClass* `model' } -> `()' #}


modelSetFilter :: (ModelClass model) => model -> ModelFilterFunc -> IO ()
modelSetFilter model filterFunc = let func = {# call model_set_filter #}
                                  in withModelClass model $ \mdlPtr -> do
                                       fFuncPtr <- newModelFilterFunc filterFunc
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

rowRemoved :: Signal Model (ModelIter ->IO ())
rowRemoved = Signal (connect_OBJECT__NONE "row-removed")


onSortChanged, afterSortChanged :: Model -> IO () -> IO (ConnectId Model)
onSortChanged = connect_NONE__NONE "sort-changed" False
afterSortChanged = connect_NONE__NONE "sort-changed" True

sortChanged :: Signal Model (ModelIter ->IO ())
sortChanged = Signal (connect_OBJECT__NONE "sort-changed")

