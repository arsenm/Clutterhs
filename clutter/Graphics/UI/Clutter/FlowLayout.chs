-- -*-haskell-*-
--  Clutter FlowLayout
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

-- | FlowLayout — A reflowing layout manager
module Graphics.UI.Clutter.FlowLayout (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- * Description

-- | 'FlowLayout' is a layout manager which implements the following policy:
--
-- the preferred natural size depends on the value of the
-- "orientation" property; the layout will try to maintain all its
-- children on a single row or column;
--
-- if either the width or the height allocated are smaller than the
-- preferred ones, the layout will wrap; in this case, the preferred
-- height or width, respectively, will take into account the amount of
-- columns and rows;
--
-- each line (either column or row) in reflowing will have the size of
-- the biggest cell on that line; if the "homogeneous" property is set
-- to @False@ the actor will be allocated within that area, and if set
-- to @True@ instead the actor will be given exactly that area;
--
-- the size of the columns or rows can be controlled for both minimum
-- and maximum; the spacing can also be controlled in both columns and
-- rows.
--
-- * Figure 4. Horizontal flow layout
-- <<file:///home/matt/src/clutterhs/clutter/doc/flow-layout-horizontal.png>>
--
-- The image shows a 'FlowLayout' with the "orientation" propert set
-- to 'FlowHorizontal'.
--
-- * Figure 5. Vertical flow layout
--
-- The image shows a 'FlowLayout' with the "orientation" propert
-- set to 'FlowVertical'.
-- <<file:///home/matt/src/clutterhs/clutter/doc/flow-layout-vertical.png>>
--
-- 'FlowLayout' is available since Clutter 1.2
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
  FlowLayout,
  FlowLayoutClass,

-- * Constructors
  flowLayoutNew,

-- * Methods
  flowLayoutSetHomogeneous,
  flowLayoutGetHomogeneous,

  flowLayoutSetOrientation,
  flowLayoutGetOrientation,

  flowLayoutSetColumnSpacing,
  flowLayoutGetColumnSpacing,

  flowLayoutSetRowSpacing,
  flowLayoutGetRowSpacing,

  flowLayoutSetColumnWidth,
  flowLayoutGetColumnWidth,

  flowLayoutSetRowHeight,
  flowLayoutGetRowHeight,

-- * Attributes
  flowLayoutColumnSpacing,
  flowLayoutHomogeneous,
  flowLayoutMaxColumnWidth,
  flowLayoutMaxRowHeight,
  flowLayoutMinColumnWidth,
  flowLayoutMinRowHeight,
  flowLayoutOrientation,
  flowLayoutRowSpacing
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


-- | Creates a new 'FlowLayout' with the given orientation
--
-- [@orientation@] the orientation of the flow layout
--
-- [@Returns@] the newly created 'FlowLayout'
--
-- * Since 1.2
--
{# fun unsafe flow_layout_new as ^
  { cFromEnum `FlowOrientation' } -> `FlowLayout' newFlowLayout* #}


-- | Sets whether the layout should allocate the same space for each
-- child
--
-- [@layout@] a 'FlowLayout'
--
-- [@homogeneous@] whether the layout should be homogeneous or not
--
-- * Since 1.2
--
{# fun unsafe flow_layout_set_homogeneous as ^
  { withFlowLayout* `FlowLayout', `Bool'} -> `()' #}


-- | Retrieves whether the layout is homogeneous
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] @True@ if the 'FlowLayout' is homogeneous
--
-- * Since 1.2
--
{# fun unsafe flow_layout_get_homogeneous as ^
  { withFlowLayout* `FlowLayout' } -> `Bool' #}



-- | Sets the orientation of the flow layout
--
-- The orientation controls the direction used to allocate the
-- children: either horizontally or vertically. The orientation also
-- controls the direction of the overflowing
--
-- [@layout@] a 'FlowLayout'
--
-- [@orientation@] the orientation of the layout
--
-- * Since 1.2
--
{# fun unsafe flow_layout_set_orientation as ^
  { withFlowLayout* `FlowLayout', cFromEnum `FlowOrientation'} -> `()' #}


-- | Retrieves the orientation of the layout
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] the orientation of the 'FlowLayout'
--
-- * Since 1.2
--
{# fun unsafe flow_layout_get_orientation as ^
  { withFlowLayout* `FlowLayout' } -> `FlowOrientation' cToEnum #}


-- | Sets the space between columns, in pixels
--
-- [@layout@] a 'FlowLayout'
--
-- [@spacing@] the space between columns
--
-- * Since 1.2
--
{# fun unsafe flow_layout_set_column_spacing as ^
  { withFlowLayout* `FlowLayout', `Float' } -> `()' #}


-- | Retrieves the spacing between columns
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] the spacing between columns of the 'FlowLayout', in
-- pixels
--
-- * Since 1.2
--
{# fun unsafe flow_layout_get_column_spacing as ^
  { withFlowLayout* `FlowLayout' } -> `Float' #}


-- | Sets the spacing between rows, in pixels
--
-- [@layout@] a 'FlowLayout'
--
-- [@spacing@] the space between rows
--
-- * Since 1.2
--
{# fun unsafe flow_layout_set_row_spacing as ^
   { withFlowLayout* `FlowLayout', `Float' } -> `()' #}


-- | Retrieves the spacing between rows
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] the spacing between rows of the 'FlowLayout', in pixels
--
-- * Since 1.2
--
{# fun unsafe flow_layout_get_row_spacing as ^
   { withFlowLayout* `FlowLayout' } -> `Float' #}


-- | Retrieves the minimum and maximum column widths
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] (min_width, max_width)
--
-- * Since 1.2
--
{# fun unsafe flow_layout_set_column_width as ^
  { withFlowLayout* `FlowLayout', `Float', `Float' } -> `()' #}


-- | Retrieves the minimum and maximum column widths
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] (min_width, max_width)
--
-- * Since 1.2
--
{# fun unsafe flow_layout_get_column_width as ^
  { withFlowLayout* `FlowLayout', alloca- `Float', alloca- `Float' } -> `()' #}


-- | Sets the minimum and maximum heights that a row can have
--
-- [@layout@] a 'FlowLayout'
--
-- [@min_height@] the minimum height of a row
--
-- [@max_height@] the maximum height of a row
--
-- * Since 1.2
--
{# fun unsafe flow_layout_set_row_height as ^
  { withFlowLayout* `FlowLayout', `Float', `Float' } -> `()' #}


-- | Retrieves the minimum and maximum row heights
--
-- [@layout@] a 'FlowLayout'
--
-- [@Returns@] (min_height, max_height)
--
-- * Since 1.2
--
{# fun unsafe flow_layout_get_row_height as ^
  { withFlowLayout* `FlowLayout', alloca- `Float', alloca- `Float' } -> `()' #}



-- | The spacing between columns, in pixels; the value of this
-- property is honoured by horizontal non-overflowing layouts and by
-- vertical overflowing layouts
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 1.2
--
flowLayoutColumnSpacing :: Attr FlowLayout Float
flowLayoutColumnSpacing = newNamedAttr "max-column-width" flowLayoutGetColumnSpacing flowLayoutSetColumnSpacing


-- | Whether each child inside the 'FlowLayout' should receive the
-- same allocation
--
-- Default value: @False@
--
-- * Since 1.2
--
flowLayoutHomogeneous :: Attr FlowLayout Bool
flowLayoutHomogeneous = newNamedAttr "homogeneous" flowLayoutGetHomogeneous flowLayoutSetHomogeneous


-- | Maximum width for each column in the layout, in pixels. If set to
-- -1 the width will be the maximum child width
--
-- Allowed values: >= -1
--
-- Default value: -1
--
-- * Since 1.2
--
flowLayoutMaxColumnWidth :: Attr FlowLayout Float
flowLayoutMaxColumnWidth = newAttrFromFloatProperty "max-column-width"


-- | Maximum height for each row in the layout, in pixels. If set to
-- -1 the width will be the maximum child height
--
-- Allowed values: >= -1
--
-- Default value: -1
--
-- * Since 1.2
--
flowLayoutMaxRowHeight :: Attr FlowLayout Float
flowLayoutMaxRowHeight = newAttrFromFloatProperty "max-row-height"


-- | Minimum width for each column in the layout, in pixels
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 1.2
--
flowLayoutMinColumnWidth :: Attr FlowLayout Float
flowLayoutMinColumnWidth = newAttrFromFloatProperty "min-column-width"


-- | Minimum height for each row in the layout, in pixels
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 1.2
--
flowLayoutMinRowHeight :: Attr FlowLayout Float
flowLayoutMinRowHeight = newAttrFromFloatProperty "min-row-height"


-- | The orientation of the 'FlowLayout'. The children of the layout
-- will be layed out following the orientation.
--
-- This property also controls the overflowing directions
--
-- Default value: 'FlowHorizontal'
--
-- * Since 1.2
--
flowLayoutOrientation :: Attr FlowLayout FlowOrientation
flowLayoutOrientation = newNamedAttr "orientation" flowLayoutGetOrientation flowLayoutSetOrientation


-- | The spacing between rows, in pixels; the value of this property
-- is honoured by vertical non-overflowing layouts and by horizontal
-- overflowing layouts
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 1.2
--
flowLayoutRowSpacing :: Attr FlowLayout Float
flowLayoutRowSpacing = newNamedAttr "row-spacing" flowLayoutGetRowSpacing flowLayoutSetRowSpacing

#endif

