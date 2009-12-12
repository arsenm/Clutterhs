-- -*-haskell-*-
--  Clutter Enums
--
--  Author : Matthew Arsenault
--
--  Created: 12 Dec 2009
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

--CHECKME: gtk2hs change broke everything. I need to double check what
--they are doing to get the referencing right, but I just want it to
--compile right now.

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
  FontFlags(..)
  ) where


--RGBData stuff

import Prelude hiding (Nothing)

import Data.Ix
import Data.Array.Storable
import Data.Array.MArray

import C2HS hiding (newForeignPtr)
import System.Glib.GObject
import System.Glib.Signals
import System.Glib.GValue (GValue(GValue))
import System.Glib.GList
import System.Glib.Flags
import System.Glib.FFI
import Control.Monad (when, liftM2, join)
import Control.Exception (bracket)


{# enum ClutterInitError as InitError {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterPickMode as PickMode {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterAllocationFlags as AllocationFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}
{# enum ClutterGravity as Gravity {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterActorFlags as ActorFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}
{# enum ClutterRequestMode as RequestMode {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterRotateAxis as RotateAxis {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterEventType as EventType {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterEventFlags as EventFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}
{# enum ClutterModifierType as ModifierType {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterStageState as StageState {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterScrollDirection as ScrollDirection {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterTimelineDirection as TimelineDirection {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterAnimationMode as AnimationMode {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterRotateDirection as RotateDirection {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterTextureQuality as TextureQuality {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterTextureFlags as TextureFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}
{# enum ClutterShaderError as ShaderError {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterPathNodeType as PathNodeType {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterUnitType as UnitType {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterInputDeviceType as InputDeviceType {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterScriptError as ScriptError {underscoreToCase} deriving (Show, Eq) #}
{# enum ClutterFontFlags as FontFlags {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance Flags EventFlags
instance Flags ActorFlags
instance Flags TextureFlags
instance Flags AllocationFlags
instance Flags FontFlags


