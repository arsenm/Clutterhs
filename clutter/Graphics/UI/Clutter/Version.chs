-- -*-haskell-*-
--  Clutter Versioning
--
--  Author : Matthew Arsenault
--
--  Created: 4 Apr 2010
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
#include <clutter-macros.h>

{# context lib="clutter" prefix="clutter" #}

-- | Versioning Macros — Versioning utility macros
module Graphics.UI.Clutter.Version (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- | Clutter offers a set of macros for checking the version of the
-- library an application was linked to.
--

-- * Versioning checks
  majorVersion,
  minorVersion,
  microVersion,
  checkVersion
#endif
  ) where

import C2HS

-- Version check for version checks. I find this entertaining.

#if CLUTTER_CHECK_VERSION(1,2,0)

-- | The major component of the Clutter library version, e.g. 1 if the
-- version is 1.2.3
--
-- * Since 1.2
--
{# fun unsafe clutterhs_clutter_major_version as majorVersion { } -> `Int' #}


-- | The minor component of the Clutter library version, e.g. 2 if the
-- version is 1.2.3
--
-- * Since 1.2
--
{# fun unsafe clutterhs_clutter_minor_version as minorVersion { } -> `Int' #}


-- | The micro component of the Clutter library version, e.g. 3 if the
-- version is 1.2.3
--
-- * Since 1.2
--
{# fun unsafe clutterhs_clutter_micro_version as microVersion { } -> `Int' #}


-- | Run-time version check, to check the version the Clutter library
-- that an application is currently linked against
--
--
-- [@major@] major version, like 1 in 1.2.3
--
-- [@minor@] minor version, like 2 in 1.2.3
--
-- [@micro@] micro version, like 3 in 1.2.3
--
-- [@Returns@] @True@ if the version of the Clutter library is greater
-- than (major, minor, micro), and @False@ otherwise
--
-- * Since 1.2
--
{# fun unsafe check_version as ^ { `Int', `Int', `Int' } -> `Bool' #}

#endif

