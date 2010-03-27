-- -*-haskell-*-
--  Clutter BoxLayout
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

module Graphics.UI.Clutter.BoxLayout (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- * Description
-- | The 'BoxLayout' is a 'LayoutManager' implementing the following
-- layout policy:
--
-- * all children are arranged on a single line;
--
-- * the axis used is controlled by the 'boxLayoutVertical' boolean property;
--
-- * the order of the packing is determined by the
-- 'boxLayoutPackStart' boolean property;
--
-- * each child will be allocated to its natural size or, if set to
-- expand, the available size;
--
-- * if a child is set to fill on either (or both) axis, its
-- allocation will match all the available size; the fill layout
-- property only makes sense if the expand property is also set;
--
-- * if a child is set to expand but not to fill then it is possible
-- to control the alignment using the X and Y alignment layout
-- properties.
--
-- * Figure 6. Box layout
--
-- The image shows a 'BoxLayout' with the 'boxLayoutVertical' property
-- set to @False@.
-- <<file:///home/matt/src/clutterhs/clutter/doc/box-layout.png>>
--
-- It is possible to control the spacing between children of a
-- 'BoxLayout' by using 'boxLayoutSetSpacing'.
--
-- In order to set the layout properties when packing an actor inside
-- a 'BoxLayout' you should use the 'boxLayoutPack' function.
--
-- * 'BoxLayout' is available since Clutter 1.2
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
  BoxLayout,
  BoxLayoutClass,

-- * Constructors
  boxLayoutNew,

-- * Methods
  boxLayoutSetPackStart,
  boxLayoutGetPackStart,
  boxLayoutSetSpacing,
  boxLayoutGetSpacing,
  boxLayoutSetVertical,
  boxLayoutGetVertical,
  boxLayoutPack,
  boxLayoutSetAlignment,
  boxLayoutGetAlignment,
  boxLayoutSetExpand,
  boxLayoutGetExpand,
  boxLayoutSetFill,
  boxLayoutGetFill,
  boxLayoutSetUseAnimations,
  boxLayoutGetUseAnimations,
  boxLayoutSetEasingDuration,
  boxLayoutGetEasingDuration,
  boxLayoutSetEasingMode,
  boxLayoutGetEasingMode,

-- * Attributes
  boxLayoutEasingDuration,
  boxLayoutEasingMode,
  boxLayoutPackStart,
  boxLayoutSpacing,
  boxLayoutUseAnimations,
  boxLayoutVertical
#endif
  ) where

#if CLUTTER_CHECK_VERSION(1,2,0)

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import System.Glib.Attributes
import System.Glib.Properties

import C2HS


-- | Creates a new 'BoxLayout' layout manager
--
-- [@Returns@] the newly created 'BoxLayout'
--
-- * Since 1.2
--
{# fun unsafe box_layout_new as ^ { } -> `BoxLayout' newBoxLayout* #}


-- | Sets whether children of layout should be layed out by appending
-- them or by prepending them
--
-- [@layout@] a 'BoxLayout'
--
-- [@pack_start@] @True@ if the layout should pack children at the
-- beginning of the layout
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_pack_start as ^
  { withBoxLayout* `BoxLayout', `Bool' } -> `()' #}

-- | Retrieves the value set using 'boxlayoutSetPackStart'
--
-- [@layout@] a 'BoxLayout'
--
-- [@Returns@] @True@ if the ClutterBoxLayout should pack children at
-- the beginning of the layout, and @False@ otherwise
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_pack_start as ^
  { withBoxLayout* `BoxLayout' } -> `Bool' #}


-- | Sets the spacing between children of layout
--
-- [@layout@] a 'BoxLayout'
--
-- [@spacing@] the spacing between children of the layout, in pixels
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_spacing as ^
  { withBoxLayout* `BoxLayout', cIntConv `Word' } -> `()' #}



-- | Retrieves the spacing set using 'boxLayoutSetSpacing'
--
-- [@layout@] a 'BoxLayout'
--
-- [@Returns@] the spacing between children of the 'BoxLayout'
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_spacing as ^
  { withBoxLayout* `BoxLayout' } -> `Word' cIntConv #}


-- | Sets whether layout should arrange its children vertically
-- alongside the Y axis, instead of horizontally alongside the X axis
--
-- [@layout@] a 'BoxLayout'
--
-- [@vertical@] @True@ if the layout should be vertical
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_vertical as ^
  { withBoxLayout* `BoxLayout', `Bool' } -> `()' #}


-- | Retrieves the orientation of the layout as set using the
-- 'boxLayoutSetVertical' function
--
-- [@layout@] a 'BoxLayout'
--
-- [@Returns@] @True@ if the 'BoxLayout' is arranging its children
-- vertically, and @False@ otherwise
--
--  * Since 1.2
--
{# fun unsafe box_layout_get_vertical as ^
  { withBoxLayout* `BoxLayout' } -> `Bool' #}



-- | Packs actor inside the 'Container' associated to layout and sets
-- the layout properties
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor
--
-- [@expand@] whether the actor should expand
--
-- [@x_fill@] whether the actor should fill horizontally
--
-- [@y_fill@] whether the actor should fill vertically
--
-- [@x_align@] the horizontal alignment policy for actor
--
-- [@y_align@] the vertical alignment policy for actor
--
-- * Since 1.2
--
{# fun unsafe box_layout_pack as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor',
    `Bool',
    `Bool',
    `Bool',
    cFromEnum `BoxAlignment',
    cFromEnum `BoxAlignment'  } -> `()' #}


-- | Sets the horizontal and vertical alignment policies for actor
-- inside layout
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor child of layout
--
-- [@x_align@] Horizontal alignment policy for actor
--
-- [@y_align@] Vertical alignment policy for actor
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_alignment as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor',
    cFromEnum `BoxAlignment',
    cFromEnum `BoxAlignment'  } -> `()' #}


-- | Retrieves the horizontal and vertical alignment policies for
-- actor as set using 'boxLayoutPack' or 'boxLayoutSetAlignment'
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor child of layout
--
-- [@Returns@] (x_align, y_align)
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_alignment as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor',
    alloca- `BoxAlignment' peekEnum*,
    alloca- `BoxAlignment' peekEnum*  } -> `()' #}


-- | Sets whether actor should expand inside layout
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor child of layout
--
-- [@expand@] whether actor should expand
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_expand as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor',
    `Bool' } -> `()' #}


-- | Retrieves whether actor should expand inside layout
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor child of layout
--
-- [@Returns@] @True@ if the Actor should expand, @False@ otherwise
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_expand as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor' } -> `Bool' #}


-- | Sets the horizontal and vertical fill policies for actor inside
-- layout
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor child of layout
--
-- [@x_fill@] whether actor should fill horizontally the allocated
-- space
--
-- [@y_fill@] whether actor should fill vertically the allocated space
--
-- * Since 1.2
--

{# fun unsafe box_layout_set_fill as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor',
    `Bool',
    `Bool'
  } -> `()' #}


-- | Retrieves the horizontal and vertical fill policies for actor as
-- set using 'boxLayoutPack' or 'boxLayoutSetFill'
--
-- [@layout@] a 'BoxLayout'
--
-- [@actor@] an Actor child of layout
--
-- [@Returns@] (horizontal fill policy, vertical fill policy)
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_fill as ^ `(ActorClass actor)' =>
  { withBoxLayout* `BoxLayout',
    withActorClass* `actor',
    alloca- `Bool',
    alloca- `Bool'
  } -> `()' #}



-- | Sets whether layout should animate changes in the layout
-- properties
--
-- The duration of the animations is controlled by
-- 'boxLayoutSetEasingDuration'; the easing mode to be used by the
-- animations is controlled by 'boxLayoutSetEasingMode'
--
-- [@layout@] a 'BoxLayout'
--
-- [@animate@] @True@ if the layout should use animations
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_use_animations as ^
  { withBoxLayout* `BoxLayout', `Bool'} -> `()' #}


-- | Retrieves whether layout should animate changes in the layout
-- properties
--
-- Since 'boxLayoutSetUseAnimations'
--
-- [@layout@] a 'BoxLayout'
--
-- [@Returns@] @True@ if the animations should be used, @False@
-- otherwise
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_use_animations as ^
  { withBoxLayout* `BoxLayout'} -> `Bool' #}


-- | Sets the duration of the animations used by layout when animating
-- changes in the layout properties
--
-- Use 'boxLayoutSetUseAnimations' to enable and disable the
-- animations
--
-- [@layout@] a 'BoxLayout'
--
-- [@msecs@] the duration of the animations, in milliseconds
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_easing_duration as ^
  { withBoxLayout* `BoxLayout', cIntConv `Word'} -> `()' #}


-- | Retrieves the duration set using 'boxLayoutSetEasingDuration'
--
-- [@layout@] a 'BoxLayout'
--
-- [@Returns@] the duration of the animations, in milliseconds
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_easing_duration as ^
  { withBoxLayout* `BoxLayout' } -> `Word' cIntConv #}


-- | Sets the easing mode to be used by layout when animating changes
-- in layout properties
--
-- Use 'boxLayoutSetUseAnimations' to enable and disable the
-- animations
--
-- [@layout@] a 'BoxLayout'
--
-- [@mode@] an easing mode, either from 'AnimationMode' or a logical
-- id from 'alphaRegisterFunc'
--
-- * Since 1.2
--
{# fun unsafe box_layout_set_easing_mode as ^
  { withBoxLayout* `BoxLayout', cFromEnum `AnimationMode'} -> `()' #}


-- | Retrieves the easing mode set using 'boxLayoutSetEasingMode'
--
-- [@layout@] a 'BoxLayout'
--
-- [@Returns@] an easing mode
--
-- * Since 1.2
--
{# fun unsafe box_layout_get_easing_mode as ^
  { withBoxLayout* `BoxLayout' } -> `AnimationMode' cToEnum #}



-- | The duration of the animations, in case 'boxLayoutUseAnimations'
-- is set to @True@
--
-- The duration is expressed in milliseconds
--
-- Default value: 500
--
-- * Since 1.2
--
boxLayoutEasingDuration :: Attr BoxLayout Word
boxLayoutEasingDuration = newNamedAttr "easing-duration" boxLayoutGetEasingDuration boxLayoutSetEasingDuration


-- | The easing mode for the animations, in case
-- 'boxLayoutUseAnimations' is set to @True@
--
-- The easing mode has the same semantics of "mode:" it can either be
-- a value from the 'AnimationMode' enumeration, like 'EaseOutCubic',
-- or a logical id as returned by 'alphaRegisterFunc'
--
-- The default value is 'EaseOutCubic'
--
-- * Since 1.2
--
boxLayoutEasingMode :: Attr BoxLayout AnimationMode
boxLayoutEasingMode = newNamedAttr "easing-mode" boxLayoutGetEasingMode boxLayoutSetEasingMode

-- | Whether the 'BoxLayout' should pack items at the start or append
-- them at the end
--
-- Default value: @False@
--
-- * Since 1.2
--
boxLayoutPackStart :: Attr BoxLayout Bool
boxLayoutPackStart = newNamedAttr "pack-start" boxLayoutGetPackStart boxLayoutSetPackStart


-- | The spacing between children of the 'BoxLayout', in pixels
--
-- Default value: 0
--
-- * Since 1.2
--
boxLayoutSpacing :: Attr BoxLayout Word
boxLayoutSpacing = newNamedAttr "spacing" boxLayoutGetSpacing boxLayoutSetSpacing


-- | Whether the 'BoxLayout' should animate changes in the layout
-- properties
--
-- Default value: @False@
--
-- * Since 1.2
--
boxLayoutUseAnimations :: Attr BoxLayout Bool
boxLayoutUseAnimations = newNamedAttr "use-animations" boxLayoutGetUseAnimations boxLayoutSetUseAnimations


-- | Whether the 'BoxLayout' should arrange its children alongside the
-- Y axis, instead of alongside the X axis
--
-- Default value: @False@
--
-- * Since 1.2
--
boxLayoutVertical :: Attr BoxLayout Bool
boxLayoutVertical = newNamedAttr "vertical" boxLayoutGetVertical boxLayoutSetVertical

#endif

