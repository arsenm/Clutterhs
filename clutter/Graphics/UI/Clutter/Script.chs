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


-- * Constructors
  scriptNew,

-- * Methods

--scriptLoadFromData,
--scriptLoadFromFile,
--scriptAddSearchPaths,
  scriptLookupFilename,
--scriptGetObject,
--scriptGetObjects,
--scriptUnmergeObjects,
  scriptEnsureObjects,
--scriptListObjects,
  scriptConnectSignals,
--scriptConnectSignalsFull,
--scriptGetTypeFromName,
  getScriptId,

-- * Attributes
  scriptFilename,
  scriptFilenameSet
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS
import Control.Monad (liftM)
import System.Glib.GObject
import System.Glib.GType
import System.Glib.GTypeConstants
import System.Glib.Attributes
import System.Glib.Properties

{# fun unsafe script_new as ^ { } -> `Script' newScript* #}

{# fun unsafe script_lookup_filename as ^ { withScript* `Script', `String' } -> `String' peekNFreeString* #}

{# fun unsafe script_unmerge_objects as ^ { withScript* `Script', cIntConv `Word' } -> `()' #}
{# fun unsafe script_ensure_objects as ^ { withScript* `Script' } -> `()' #}


--CHECKME: unsafe probably wrong
scriptConnectSignals :: Script -> IO ()
scriptConnectSignals script = withScript script $ \p ->
                              {# call unsafe script_connect_signals #} p nullPtr

{-
scriptConnectSignalsFull :: Script -> ScriptConnectFunc -> IO ()
scriptConnectSignals script func = withScript $ \p -> do
                                     funcPtr <- mkScriptConnectFunc func
                                     {# call unsafe script_connect_signals #} p funcPtr nullPtr
                                     freeHaskellFunPtr funcPtr
-}


{# fun unsafe script_get_type_from_name as ^
   { withScript* `Script', `String' } -> `GType' cFromEnum #}

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


