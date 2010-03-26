-- -*-haskell-*-
--  Clutter FixedLayout
--
--  Author : Matthew Arsenault
--
--  Created: 26 Mar 2010
--
--  Copyright (C) 2010 Matthew Arsenault
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

-- | FixedLayout — A fixed layout manager
module Graphics.UI.Clutter.FixedLayout (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- * Description
-- | FixedLayout is a layout manager implementing the same layout
-- policies as 'Group'.
--
-- 'FixedLayout' is available since Clutter 1.2
--

-- * Class Hierarchy
-- |
-- @
-- |    'GObject'
-- |     +----GInitiallyUnowned
-- |           +----'LayoutManager'
-- |                  +----'FixedLayout'
-- |                  +----'BinLayout'
-- |                  +----'FlowLayout'
-- |                  +----'BoxLayout'
-- @
--

-- * Types
  FixedLayout,
  FixedLayoutClass,

-- * Constructors
  fixedLayoutNew

#endif
  ) where

#if CLUTTER_CHECK_VERSION(1,2,0)

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import C2HS

-- | Creates a new 'FixedLayout'
--
-- [@Returns@] the newly created 'FixedLayout'
--
-- * Since 1.2
--
{# fun unsafe fixed_layout_new as ^ { } -> `FixedLayout' newFixedLayout* #}

#endif

