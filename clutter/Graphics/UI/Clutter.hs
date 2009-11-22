-- -*-haskell-*-
--  Clutter
--
--  Author : Matthew Arsenault
--
--  Created: 5 September 2009
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
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  Lesser General Public License for more details.

-- | This module gathers all publicly available functions from the Clutter binding.
--
module Graphics.UI.Clutter (
-- * Core Reference
-- ** Types and general stuff
  module Graphics.UI.Clutter.Types,
  module Graphics.UI.Clutter.StoreValue,

-- ** Abstract classes and interfaces
  module Graphics.UI.Clutter.Actor,
  module Graphics.UI.Clutter.Container,
  module Graphics.UI.Clutter.ChildMeta,
  module Graphics.UI.Clutter.Media,

-- ** Base actors
  module Graphics.UI.Clutter.Rectangle,
  module Graphics.UI.Clutter.Texture,
  module Graphics.UI.Clutter.Clone,
  module Graphics.UI.Clutter.Text,
  module Graphics.UI.Clutter.CairoTexture,

-- ** Container actors
  module Graphics.UI.Clutter.Group,
  module Graphics.UI.Clutter.Stage,

-- * Animation Framework

-- ** Base classes
  module Graphics.UI.Clutter.Timeline,
  module Graphics.UI.Clutter.Score,
  module Graphics.UI.Clutter.Alpha,
  module Graphics.UI.Clutter.Behaviour,

-- ** Behaviours
  module Graphics.UI.Clutter.BehaviourDepth,
  module Graphics.UI.Clutter.BehaviourEllipse,
  module Graphics.UI.Clutter.BehaviourOpacity,
  module Graphics.UI.Clutter.BehaviourPath,
  module Graphics.UI.Clutter.Path,
  module Graphics.UI.Clutter.BehaviourRotate,
  module Graphics.UI.Clutter.BehaviourScale,

-- ** High Level API
  module Graphics.UI.Clutter.Interval,
  module Graphics.UI.Clutter.Animation,
  module Graphics.UI.Clutter.Animatable,

-- * Clutter Tools
-- ** General purpose API
  module Graphics.UI.Clutter.Color,
  module Graphics.UI.Clutter.BindingPool,
  module Graphics.UI.Clutter.Event,
  module Graphics.UI.Clutter.General,
  module Graphics.UI.Clutter.Shader,
  module Graphics.UI.Clutter.Units,
  module Graphics.UI.Clutter.Utilities,
--module Graphics.UI.Clutter.Versioning,

-- ** User interface definition
  module Graphics.UI.Clutter.Script,
  module Graphics.UI.Clutter.Scriptable,

-- ** Generic list model
  module Graphics.UI.Clutter.Model,
  module Graphics.UI.Clutter.ModelIter,
  module Graphics.UI.Clutter.ListModel,
-- * Backends
  module Graphics.UI.Clutter.X11,
--module Graphics.UI.Clutter.GLX,
--module Graphics.UI.Clutter.Win32,
--module Graphics.UI.Clutter.EGL,
--module Graphics.UI.Clutter.EGLX
  ) where

import Graphics.UI.Clutter.Event
import Graphics.UI.Clutter.Stage
import Graphics.UI.Clutter.Types
import Graphics.UI.Clutter.Color
import Graphics.UI.Clutter.Actor
import Graphics.UI.Clutter.General
import Graphics.UI.Clutter.Rectangle
import Graphics.UI.Clutter.Group
import Graphics.UI.Clutter.Container
import Graphics.UI.Clutter.Texture
import Graphics.UI.Clutter.Text
import Graphics.UI.Clutter.Animation
import Graphics.UI.Clutter.Animatable
import Graphics.UI.Clutter.Timeline
import Graphics.UI.Clutter.Score
import Graphics.UI.Clutter.StoreValue
import Graphics.UI.Clutter.Alpha
import Graphics.UI.Clutter.CairoTexture
import Graphics.UI.Clutter.Media
import Graphics.UI.Clutter.ChildMeta
import Graphics.UI.Clutter.Clone
import Graphics.UI.Clutter.Behaviour
import Graphics.UI.Clutter.BehaviourScale
import Graphics.UI.Clutter.BehaviourDepth
import Graphics.UI.Clutter.BehaviourEllipse
import Graphics.UI.Clutter.BehaviourOpacity
import Graphics.UI.Clutter.BehaviourRotate
import Graphics.UI.Clutter.BehaviourPath
import Graphics.UI.Clutter.Interval
import Graphics.UI.Clutter.Path
import Graphics.UI.Clutter.Shader
import Graphics.UI.Clutter.Model
import Graphics.UI.Clutter.ModelIter
import Graphics.UI.Clutter.Script
import Graphics.UI.Clutter.Units
import Graphics.UI.Clutter.Scriptable
import Graphics.UI.Clutter.BindingPool
import Graphics.UI.Clutter.X11
import Graphics.UI.Clutter.Utilities
import Graphics.UI.Clutter.ListModel


