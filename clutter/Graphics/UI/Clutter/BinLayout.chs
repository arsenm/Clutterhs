-- -*-haskell-*-
--  Clutter BinLayout
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

-- | BinLayout — A simple layout manager
module Graphics.UI.Clutter.BinLayout (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- * Description
-- | 'BinLayout' is a layout manager which implements the following
-- policy:
--
-- the preferred size is the maximum preferred size between all the
-- children of the container using the layout; each child is allocated
-- in "layers", on on top of the other; for each layer there are
-- horizontal and vertical alignment policies.
--
-- * Figure 3. Bin layout
-- <<file:///home/matt/src/clutterhs/clutter/doc/bin-layout.png>>
--
-- The image shows a 'BinLayout' with three layers: a background
-- 'CairoTexture', set to fill on both the X and Y axis; a 'Texture',
-- set to center on both the X and Y axis; and a 'Rectangle', set to
-- 'BinAlignmentEnd' on both the X and Y axis.
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
  BinLayout,
  BinLayoutClass,
  BinAlignment(..),

-- * Constructors
  binLayoutNew,

-- * Methods
  binLayoutSetAlignment,
  binLayoutGetAlignment,
  binLayoutAdd,

-- * Attributes
  binLayoutXAlign,
  binLayoutYAlign
#endif
  ) where

#if CLUTTER_CHECK_VERSION(1,2,0)

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Utility #}
{# import qualified Graphics.UI.Clutter.GTypes #} as CGT

import System.Glib.Attributes
import System.Glib.Properties

import C2HS

-- | Creates a new 'BinLayout' layout manager
--
-- [@x_align@] the default alignment policy to be used on the horizontal axis
--
-- [@y_align@] the default alignment policy to be used on the vertical axis
--
-- [@Returns@] the newly created layout manager
--
-- * Since 1.2
--
{# fun unsafe bin_layout_new as ^ { cFromEnum `BinAlignment',
                                    cFromEnum `BinAlignment' } -> `BinLayout' newBinLayout* #}


-- | Sets the horizontal and vertical alignment policies to be applied
-- to a child of self
--
-- If child is @Nothing@ then the x_align and y_align values will be
-- set as the default alignment policies
--
-- [@self@] a 'BinLayout'
--
-- [@child@] Maybe a child of container.
--
-- [@x_align@] the horizontal alignment policy to be used for the
-- child inside container
--
-- [@y_align@] the vertical aligment policy to be used on the child
-- inside container
--
-- * Since 1.2
--
{# fun unsafe bin_layout_set_alignment as ^ `(ActorClass child)' => { withBinLayout* `BinLayout',
                                                                      withMaybeActorClass* `Maybe child',
                                                                      cFromEnum `BinAlignment',
                                                                      cFromEnum `BinAlignment'
                                                                    } -> `()' #}


-- | Retrieves the horizontal and vertical alignment policies for a
-- child of self
--
-- If  child  is @Nothing@  the  default  alignment  policies will  be
-- returned instead
--
-- [@self@] a 'BinLayout'
--
-- [@child@] Maybe a child of container.
--
-- @Returns@] (x_align, y_align)
--
-- * Since 1.2
--
{# fun unsafe bin_layout_get_alignment as ^ `(ActorClass child)' => { withBinLayout* `BinLayout',
                                                                      withMaybeActorClass* `Maybe child',
                                                                      alloca- `BinAlignment' peekEnum*,
                                                                      alloca- `BinAlignment' peekEnum*
                                                                    } -> `()' #}


-- | Adds an Actor to the container using self and sets the alignment
-- policies for it
--
-- This function is equivalent to 'containerAddActor' and
-- 'layoutManagerChildSetProperty' but it does not require a pointer
-- to the 'Container' associated to the 'BinLayout'
--
-- [@self@] a 'BinLayout'
--
-- [@child@] an Actor
--
-- [@x_align@] horizontal alignment policy for child
--
-- [@y_align@] vertical alignment policy for child
--
-- * Since 1.2
--
{# fun unsafe bin_layout_add as ^ `(ActorClass child)' => { withBinLayout* `BinLayout',
                                                            withActorClass* `child',
                                                            cFromEnum `BinAlignment',
                                                            cFromEnum `BinAlignment'
                                                          } -> `()' #}

binLayoutXAlign :: Attr BinLayout BinAlignment
binLayoutXAlign = newAttrFromEnumProperty "x-align" CGT.binLayout

binLayoutYAlign :: Attr BinLayout BinAlignment
binLayoutYAlign = newAttrFromEnumProperty "y-align" CGT.binLayout


#endif

