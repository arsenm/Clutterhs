-- -*-haskell-*-
--  Clutter Color
--
--  Author : Matthew Arsenault
--
--  Created: 4 Sep 2009
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

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Color (
                                  colorNew,
                                  colorCopy
                                 ) where

{#import Graphics.UI.Clutter.Types#}

-- | Creates a new color
--
colorNew :: IO ClutterColor
colorNew = undefined
--  {# call unsafe color_new #} --arguments?

colorCopy :: ClutterColor -> IO ClutterColor
colorCopy = undefined


