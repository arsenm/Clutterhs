-- -*-haskell-*-
--  Clutter Enums
--
--  Author : Matthew Arsenault
--
--  Created: 12 Dec 2009
--
--  Copyright (C) 2009-2010 Matthew Arsenault
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

module Graphics.UI.Clutter.Enums (
  PickMode(..),
  Gravity(..),
  RequestMode(..),
  ActorFlags(..),
  AllocationFlags(..),
  RotateAxis(..),
  InitError(..),

  EventType(..),
  EventFlags(..),
  ModifierType(..),
  StageState(..),
  ScrollDirection(..),

  AnimationMode(..),
  TimelineDirection(..),


  RotateDirection(..),
  TextureQuality(..),
  TextureFlags(..),
  ShaderError(..),

  PathNodeType(..),
  UnitType(..),

  InputDeviceType(..),
  ScriptError(..),
  FontFlags(..),

#if CLUTTER_CHECK_VERSION(1,2,0)
  BinAlignment(..),
  FlowOrientation(..)
#endif
  ) where


--RGBData stuff

import Prelude hiding (Nothing)
import System.Glib.Flags


-- | Error conditions returned by 'clutterInit'
--
-- [@InitSuccess@] Initialisation successful
--
-- [@InitErrorUnknown@] Unknown error
--
-- [@InitErrorThreads@] Thread initialisation failed
--
-- [@InitErrorBackend@] Backend initialisation failed
--
-- [@InitErrorInternal@] Internal error
--
-- * Since 0.2
--
{# enum ClutterInitError as InitError {underscoreToCase} deriving (Show, Eq) #}


-- | Controls the paint cycle of the scene graph when in pick mode
--
-- [@PickNone@] Do not paint any actor
--
-- [@PickReactive@] Paint only the reactive actors
--
-- [@PickAll@] Paint all actors
--
-- * Since 1.0
--
{# enum ClutterPickMode as PickMode {underscoreToCase} deriving (Show, Eq) #}


-- | Flags passed to the allocate() virtual function and to the
-- 'actorAllocate' function
--
-- [@AllocationNone@] No flag set
--
-- [@AbsoluteOriginChanged@] Whether the absolute origin of the actor
-- has changed; this implies that any ancestor of the actor has been
-- moved
--
-- * Since 1.0
--
{# enum ClutterAllocationFlags as AllocationFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}


-- | Gravity of the scaling operations. When a gravity different than
-- 'GravityNone' is used, an actor is scaled keeping the position of
-- the specified portion at the same coordinates.
--
-- [@GravityNone@] Do not apply any gravity
--
-- [@GravityNorth@] Scale from topmost downwards
--
-- [@GravityNorthEast@] Scale from the top right corner
--
-- [@GravityEast@] Scale from the right side
--
-- [@GravitySouthEast@] Scale from the bottom right corner
--
-- [@GravitySouth@] Scale from the bottom upwards
--
-- [@GravitySouthWest@] Scale from the bottom left corner
--
-- [@GravityWest@] Scale from the left side
--
-- [@GravityNorthWest@] Scale from the top left corner
--
-- [@GravityCenter@] Scale from the center.
--
-- * Since 0.2
--
{# enum ClutterGravity as Gravity {underscoreToCase} deriving (Show, Eq) #}


-- | Flags used to signal the state of an actor.
--
-- [@ActorMapped@] the actor will be painted (is visible, and inside a
-- toplevel, and all parents visible)
--
-- [@ActorRealized@] the resources associated to the actor have been
-- allocated
--
-- [@ActorReactive@] the actor 'reacts' to mouse events emmitting
-- event signals
--
-- [@ActorVisible@] the actor has been shown by the application
-- program
--
{# enum ClutterActorFlags as ActorFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}


-- | Specifies the type of requests for a ClutterActor.
--
-- [@RequestHeightForWidth@] Height for width requests
--
-- [@RequestWidthForHeight@] Width for height requests
--
-- * Since 0.8
--
{# enum ClutterRequestMode as RequestMode {underscoreToCase} deriving (Show, Eq) #}


-- | Axis of a rotation.
--
-- [@XAxis@] Rotate around the X axis
--
-- [@YAxis@] Rotate around the Y axis
--
-- [@ZAxis@] Rotate around the Z axis
--
-- * Since 0.4
--
{# enum ClutterRotateAxis as RotateAxis {underscoreToCase} deriving (Show, Eq) #}


-- | Types of events.
--
-- [@Nothing@] Empty event
--
-- [@KeyPress@] Key press event
--
-- [@KeyRelease@] Key release event
--
-- [@Motion@] Pointer motion event
--
-- [@Enter@] Actor enter event
--
-- [@Leave@] Actor leave event
--
-- [@ButtonPress@] Pointer button press event
--
-- [@ButtonRelease@] Pointer button release event
--
-- [@Scroll@] Pointer scroll event
--
-- [@StageState@] Stage stage change event
--
-- [@DestroyNotify@] Destroy notification event
--
-- [@ClientMessage@] Client message event
--
-- [@Delete@] Stage delete event
--
-- * Since 0.4
--
{# enum ClutterEventType as EventType {underscoreToCase} deriving (Show, Eq) #}


-- | Flags for the 'Event'
--
-- [@EventNone@] No flag set
--
-- [@EventFlagSynthetic@] Synthetic event
--
-- * Since 0.6
--
{# enum ClutterEventFlags as EventFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}


-- | Masks applied to a 'Event' by modifiers.
--
-- [@ShiftMask@] Mask applied by the Shift key
--
-- [@LockMask@] Mask applied by the Caps Lock key
--
-- [@ControlMask@] Mask applied by the Control key
--
-- [@Mod1Mask@] Mask applied by the first Mod key
--
-- [@Mod2Mask@] Mask applied by the second Mod key
--
-- [@Mod3Mask@] Mask applied by the third Mod key
--
-- [@Mod4Mask@] Mask applied by the fourth Mod key
--
-- [@Mod5Mask@] Mask applied by the fifth Mod key
--
-- [@Button1Mask@] Mask applied by the first pointer button
--
-- [@Button2Mask@] Mask applied by the second pointer button
--
-- [@Button3Mask@] Mask applied by the third pointer button
--
-- [@Button4Mask@] Mask applied by the fourth pointer button
--
-- [@Button5Mask@] Mask applied by the fifth pointer button
--
-- [@SuperMask@] Mask applied by the Super key
--
-- [@HyperMask@] Mask applied by the Hyper key
--
-- [@MetaMask@] Mask applied by the Meta key
--
-- [@ReleaseMask@] Mask applied during release
--
-- [@ModifierMask@] A mask covering all modifier types
--
-- * Since 0.4
--
{# enum ClutterModifierType as ModifierType {underscoreToCase} deriving (Show, Eq) #}


-- | Stage state masks
--
-- [@StageStateFullscreen@] Fullscreen mask
--
-- [@StageStateOffscreen@] Offscreen mask
--
-- [@StageStateActivated@] Activated mask
--
-- * Since 0.4
--
{# enum ClutterStageState as StageState {underscoreToCase} deriving (Show, Eq) #}


-- | Direction of a pointer scroll event.
--
-- [@ScrollUp@] Scroll up
--
-- [@ScrollDown@] Scroll down
--
-- [@ScrollLeft@] Scroll left
--
-- [@ScrollRight@] Scroll right
--
-- * Since 0.4
--
{# enum ClutterScrollDirection as ScrollDirection {underscoreToCase} deriving (Show, Eq) #}


-- | The direction of a 'Timeline'
--
-- [@TimelineForward@] forward direction for a timeline
--
-- [@TimelineBackward@] backward direction for a timeline
--
-- * Since 0.6
--
{# enum ClutterTimelineDirection as TimelineDirection {underscoreToCase} deriving (Show, Eq) #}


-- | The animation modes used by 'Alpha' and 'Animation'. This
-- enumeration can be expanded in later versions of Clutter. See the
-- 'Alpha' documentation for a graph of all the animation modes.
--
-- Every global alpha function registered using 'alphaRegisterFunc'
-- will have a logical id greater than @AnimationLast@.
--
--
-- [@CustomMode@] custom progress function
--
-- [@Linear@] linear tweening
--
-- [@EaseInQuad@] quadratic tweening
--
-- [@EaseOutQuad@] quadratic tweening, inverse of 'EaseInQuad'
--
-- [@EaseInOutQuad@] quadratic tweening, combininig 'EaseInQuad' and
-- 'EaseOutQuad'
--
-- [@EaseInCubic@] cubic tweening
--
-- [@EaseOutCubic@] cubic tweening, invers of 'EaseInCubic'
--
-- [@EaseInOutCubic@] cubic tweening, combining 'EaseInCubic' and
-- 'EaseOutCubic'
--
-- [@EaseInQuart@] quartic tweening
--
-- [@EaseOutQuart@] quartic tweening, inverse of 'EaseInQuart'
--
-- [@EaseInOutQuart@] quartic tweening, combining 'EaseInQuart' and
-- 'EaseOutQuart'
--
-- [@EaseInQuint@] quintic tweening
--
-- [@EaseOutQuint@] quintic tweening, inverse of @EaseInQuint@]
--
-- [@EaseInOutQuint@] fifth power tweening, combining 'EaseInQuint'
-- and 'EaseOutQuint'
--
-- [@EaseInSine@] sinusoidal tweening
--
-- [@EaseOutSine@] sinusoidal tweening, inverse of 'EaseInSine'
--
-- [@EaseInOutSine@] sine wave tweening, combining 'EaseInSine' and
-- 'EaseOutSine'
--
-- [@EaseInExpo@] exponential tweening
--
-- [@EaseOutExpo@] exponential tweening, inverse of 'EaseInExpo'
--
-- [@EaseInOutExpo@] exponential tweening, combining 'EaseInExpo' and
-- 'EaseOutExpo'
--
-- [@EaseInCirc@] circular tweening
--
-- [@EaseOutCirc@] circular tweening, inverse of 'EaseInCirc'
--
-- [@EaseInOutCirc@] circular tweening, combining 'EaseInCirc' and
-- 'EaseOutCirc'
--
-- [@EaseInElastic@] elastic tweening, with offshoot on start
--
-- [@EaseOutElastic@] elastic tweening, with offshoot on end
--
-- [@EaseInOutElastic@] elastic tweening with offshoot on both ends
--
-- [@EaseInBack@] overshooting cubic tweening, with backtracking on
-- start
--
-- [@EaseOutBack@] overshooting cubic tweening, with backtracking on
-- end
--
-- [@EaseInOutBack@] overshooting cubic tweening, with backtracking on
-- both ends
--
-- [@EaseInBounce@] exponentially decaying parabolic (bounce)
-- tweening, with bounce on start
--
-- [@EaseOutBounce@] exponentially decaying parabolic (bounce)
-- tweening, with bounce on end
--
-- [@EaseInOutBounce@] exponentially decaying parabolic (bounce)
-- tweening, with bounce on both ends
--
-- [@AnimationLast@] last animation mode, used as a guard for
-- registered global alpha functions
--
-- * Since 1.0
--
{# enum ClutterAnimationMode as AnimationMode {underscoreToCase} deriving (Show, Eq) #}


-- | Direction of a rotation.
--
-- [@RotateCw@] Clockwise rotation
--
-- [@RotateCcw@] Counter-clockwise rotation
--
-- * Since 0.4
--
{# enum ClutterRotateDirection as RotateDirection {underscoreToCase} deriving (Show, Eq) #}


-- | Enumaration controlling the texture quality.
--
-- [@TextureQualityLow@] fastest rendering will use nearest neighbour
-- interpolation when rendering. good setting.
--
-- [@TextureQualityMedium@] higher quality rendering without using
-- extra resources.
--
-- [@TextureQualityHigh@] render the texture with the best quality
-- available using extra memory.
--
-- * Since 0.8
--
{# enum ClutterTextureQuality as TextureQuality {underscoreToCase} deriving (Show, Eq) #}


-- | Flags for 'textureSetFromRgbData' and 'textureSetFromYuvData'.
--
-- [@TextureNone@] No flags
--
-- [@TextureRgbFlagBgr@] FIXME
--
-- [@TextureRgbFlagPremult@] FIXME
--
-- [@TextureYuvFlagYuv2@] FIXME
--
-- * Since 0.4
--
{# enum ClutterTextureFlags as TextureFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}


-- | 'Shader' error enumeration
--
-- [@ShaderErrorNoAsm@] No ASM shaders support
--
-- [@ShaderErrorNoGlsl@] No GLSL shaders support
--
-- [@ShaderErrorCompile@] Compilation error
--
-- * Since 0.6
--
{# enum ClutterShaderError as ShaderError {underscoreToCase} deriving (Show, Eq) #}


-- | Types of nodes in a 'Path'.
--
-- [@PathMoveTo@] jump to the given position
--
-- [@PathLineTo@] create a line from the last node to the given
-- position
--
-- [@PathCurveTo@] bezier curve using the last position and three
-- control points.
--
-- [@PathClose@] create a line from the last node to the last
-- 'PathMoveTo' node.
--
-- [@PathRelMoveTo@] same as 'PathMoveTo' but with coordinates
-- relative to the last node.
--
-- [@PathRelLineTo@] same as 'PathLineTo' but with coordinates
-- relative to the last node.
--
-- [@PathRelCurveTo@] same as 'PathCurveTo' but with coordinates
-- relative to the last node.
--
-- * Since 1.0
--
{# enum ClutterPathNodeType as PathNodeType {underscoreToCase} deriving (Show, Eq) #}


-- | The type of unit in which a value is expressed
--
-- This enumeration might be expanded at later date
--
-- [@UnitPixel@] Unit expressed in pixels (with subpixel precision)
--
-- [@UnitEm@] Unit expressed in em
--
-- [@UnitMm@] Unit expressed in millimeters
--
-- [@UnitPoint@] Unit expressed in points
--
-- * Since 1.0
--
{# enum ClutterUnitType as UnitType {underscoreToCase} deriving (Show, Eq) #}


-- | The types of input devices available.
--
-- The 'InputDeviceType' enumeration can be extended at later date;
-- not every platform supports every input device type.
--
-- [@PointerDevice@] A pointer device
--
-- [@KeyboardDevice@] A keyboard device
--
-- [@ExtensionDevice@] A generic extension device
--
-- [@NDeviceTypes@] The number of device types
--
-- * Since 1.0
--
{# enum ClutterInputDeviceType as InputDeviceType {underscoreToCase} deriving (Show, Eq) #}


-- | 'Script' error enumeration.
--
-- [@ScriptErrorInvalidTypeFunction@] Type function not found or invalid
--
-- [@ScriptErrorInvalidProperty@] Property not found or invalid
--
-- [@ScriptErrorInvalidValue@] Invalid value
--
-- * Since 0.6
--
{# enum ClutterScriptError as ScriptError {underscoreToCase} deriving (Show, Eq) #}


-- | Runtime flags to change the font quality. To be used with
-- 'setFontFlags'.
--
-- [@FontMipmapping@] Set to use mipmaps for the glyph cache textures.
--
-- [@FontHinting@] Set to enable hinting on the glyphs.
--
-- * Since 1.0
--
{# enum ClutterFontFlags as FontFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags EventFlags
instance Flags ActorFlags
instance Flags TextureFlags
instance Flags AllocationFlags
instance Flags FontFlags


#if CLUTTER_CHECK_VERSION(1,2,0)

-- | The alignment policies available on each axis for
-- 'BinLayout'
--
-- [@BinAlignmentFixed@] Fixed position alignment; the 'BinLayout'
-- will honour the fixed position provided by the actors themselves
-- when allocating them
--
-- [@BinAlignmentFill@] Fill the allocation size
--
-- [@BinAlignmentStart@] Position the actors at the top or left side
-- of the container, depending on the axis
--
-- [@BinAlignmentEnd@] Position the actors at the bottom or right side
-- of the container, depending on the axis
--
-- [@BinAlignmentCenter@]
--
-- Position the actors at the center of the container, depending on the axis
--
-- * Since 1.2
--

{# enum ClutterBinAlignment as BinAlignment {underscoreToCase} deriving (Show, Eq, Bounded) #}


-- | The direction of the arrangement of the children inside a
-- 'FlowLayout'
--
-- [@FlowHorizontal@] Arrange the children of the flow layout
-- horizontally first
--
-- [@FlowVertical@] Arrange the children of the flow layout vertically first
--
-- * Since 1.2
--
{# enum ClutterFlowOrientation as FlowOrientation {underscoreToCase} deriving (Show, Eq, Bounded) #}

#endif

