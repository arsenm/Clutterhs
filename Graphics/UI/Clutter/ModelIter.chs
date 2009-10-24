-- -*-haskell-*-
--  Clutter ModelIter
--
--  Author : Matthew Arsenault
--
--  Created: 24 Oct 2009
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

module Graphics.UI.Clutter.ModelIter (
                                      modelIterCopy,
                                    --modelIterGet,
                                    --modelIterGetValue,
                                    --modelIterSet,
                                    --modelIterSetValue,
                                      modelIterIsFirst,
                                      modelIterIsLast,
                                      modelIterNext,
                                      modelIterPrev,
                                      modelIterGetModel,
                                      modelIterModel,
                                      modelIterGetRow,
                                      modelIterRow
                                     ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.GValue #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.GType
import qualified System.Glib.GTypeConstants as GType

{# fun unsafe model_iter_copy as ^ { withModelIter* `ModelIter' } -> `ModelIter' newModelIter* #}


{# fun unsafe model_iter_is_first as ^ { withModelIter* `ModelIter' } -> `Bool' #}
{# fun unsafe model_iter_is_last as ^ { withModelIter* `ModelIter' } -> `Bool' #}

{# fun unsafe model_iter_next as ^ { withModelIter* `ModelIter' } -> `ModelIter' newModelIter* #}

{# fun unsafe model_iter_prev as ^ { withModelIter* `ModelIter' } -> `ModelIter' newModelIter* #}

{# fun unsafe model_iter_get_model as ^ { withModelIter* `ModelIter' } -> `Model' newModel* #}
modelIterModel :: ReadAttr ModelIter Model
modelIterModel = readAttr modelIterGetModel

{# fun unsafe model_iter_get_row as ^ { withModelIter* `ModelIter' } -> `GUInt' cIntConv #}
modelIterRow :: ReadAttr ModelIter GUInt
modelIterRow = readAttr modelIterGetRow

