-- -*-haskell-*-
--  Clutter Script
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
{-# LANGUAGE ForeignFunctionInterface  #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Script (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Script'
-- @

-- * Types
  Script,
  ScriptError(..),
  MergeID,


-- * Constructors
  scriptNew,

-- * Methods
  scriptLoadFromData,
  scriptLoadFromFile,
  scriptAddSearchPaths,
  scriptLookupFilename,
--scriptGetObject,
--scriptGetObjects,
--scriptUnmergeObjects,
  scriptEnsureObjects,
--scriptListObjects,
  scriptConnectSignals,
--scriptConnectSignalsFull,
  scriptGetTypeFromName,
  getScriptId,

-- * Attributes
  scriptFilename,
  scriptFilenameSet
  ) where

{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.GType
import System.Glib.GTypeConstants
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GError

{# fun unsafe script_new as ^ { } -> `Script' newScript* #}

type MergeID = Word




-- | Loads the definitions from data into script and merges with the
-- currently loaded ones, if any.
--
-- [@script@] a 'Script'
--
-- [@data@] a string containing the definitions
--
-- on error, @Nothing@ is returned and an exception is thrown
-- accordingly. On success, the merge id for the UI definitions is
-- returned. You can use the merge id with 'scriptUnmerge'.
--
-- * Since 0.6
--
scriptLoadFromData :: Script -> String -> IO (Maybe MergeID)
scriptLoadFromData script dat = let func = {# call unsafe script_load_from_data #}
                                in withScript script $ \scrPtr ->
                                     withCString dat $ \strPtr ->
                                       propagateGError $ \gerrorPtr -> do
                                         ret <- func scrPtr strPtr (-1) gerrorPtr
                                         return $ if ret == 0
                                                    then Prelude.Nothing
                                                    else Just (cIntConv ret)




-- | Loads the definitions from filename into script and merges with
--   the currently loaded ones, if any.
--
-- [@script@] a 'Script'
--
-- [@filename@] the full path to the definition file
--
-- on error, @Nothing@ is returned and error is set accordingly. On
-- success, the merge id for the UI definitions is returned. You can
-- use the merge id with 'scriptUnmerge'.
--
-- * Since 0.6
--
scriptLoadFromFile :: Script -> String -> IO (Maybe MergeID)
scriptLoadFromFile script filename = let func = {# call unsafe script_load_from_file #}
                                     in withScript script $ \scrPtr ->
                                          withCString filename $ \strPtr ->
                                            propagateGError $ \gerrorPtr -> do
                                              ret <- func scrPtr strPtr gerrorPtr
                                              return $ if ret == 0
                                                         then Prelude.Nothing
                                                         else Just (cIntConv ret)


-- | Adds paths to the list of search paths held by script.
--
-- The search paths are used by 'scriptLookupFilename', which can be
-- used to define search paths for the textures source file name or
-- other custom, file-based properties.
--
-- [@script@] a 'Script'
--
-- [@paths@] a list of strings containing different search paths
--
-- * Since 0.8
--
scriptAddSearchPaths :: Script -> [String] -> IO ()
scriptAddSearchPaths script paths = let len = length paths
                                        func = {# call unsafe script_add_search_paths #}
                                    in withScript script $ \scrPtr ->
                                         withMany withCString paths $ \strPtrList ->
                                           withArray strPtrList $ \strPtrPtr ->
                                             func scrPtr strPtrPtr (cIntConv len)



-- | Looks up filename inside the search paths of script. If filename
-- is found, its full path will be returned .
--
-- [@script@] a 'Script'
--
-- [@filename@] the name of the file to lookup
--
-- [@Returns@] @Just@ the full path of filename or @Nothing@ if no
-- path was found.
--
-- * Since 0.8
--
scriptLookupFilename :: Script -> String -> IO (Maybe String)
scriptLookupFilename script filename = let func = {# call unsafe script_lookup_filename #}
                                       in withScript script $ \scrPtr ->
                                            withCString filename $ \fnPtr -> do
                                              ret <- func scrPtr fnPtr
                                              if ret == nullPtr
                                                 then return Prelude.Nothing
                                                 else liftM Just (peekNFreeString ret)


-- script_get_object
-- script_get_objects


-- | Unmerges the objects identified by merge_id.
--
-- [@script@] a 'Script'
--
-- [@merge_id@] merge id returned when loading a UI definition
--
-- * Since 0.6
--
{# fun unsafe script_unmerge_objects as ^ { withScript* `Script', cIntConv `Word' } -> `()' #}


-- | Ensure that every object defined inside script is correctly
-- constructed. You should rarely need to use this function.
--
-- [@script@] a 'Script'
--
-- * Since 0.6
--
{# fun unsafe script_ensure_objects as ^ { withScript* `Script' } -> `()' #}


--CHECKME: unsafe probably wrong
scriptConnectSignals :: Script -> IO ()
scriptConnectSignals script = withScript script $ \p ->
                              {# call unsafe script_connect_signals #} p nullPtr

{-
Not useful? For interpreted language bindings according to doc.
scriptConnectSignalsFull :: Script -> ScriptConnectFunc -> IO ()
scriptConnectSignals script func = withScript $ \p -> do
                                     funcPtr <- mkScriptConnectFunc func
                                     {# call unsafe script_connect_signals #} p funcPtr nullPtr
                                     freeHaskellFunPtr funcPtr
-}

-- TODO: Too low level?
{# fun unsafe script_get_type_from_name as ^
   { withScript* `Script', `String' } -> `GType' cToEnum #}

{# fun unsafe get_script_id as ^
       `(GObjectClass gobject)' => { withGObject* `gobject' } -> `String' #}



-- | The path of the currently parsed file. If 'scriptFilenameSet' is
--   @False@ then the value of this property is undefined.
--
-- Default value: @Nothing@
--
-- * Since 0.6
--
scriptFilename :: ReadAttr Script (Maybe String)
scriptFilename = readAttrFromMaybeStringProperty "filename"


-- | Whether the 'scriptFilename' property is set. If this property is
--   @True@ then the currently parsed data comes from a file, and the
--   file name is stored inside the 'Script':filename property.
--
-- Default value: @False@
--
-- * Since 0.6
--
scriptFilenameSet :: ReadAttr Script Bool
scriptFilenameSet = readAttrFromBoolProperty "filename-set"


