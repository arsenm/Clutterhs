 -- -*-haskell-*-
 --  Clutter Alpha
 --
 --  Author : Matthew Arsenault
 --
 --  Created: 22 Sep 2009
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
 {-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

-- | A class for calculating an alpha value as a function of time.
module Graphics.UI.Clutter.Alpha (
-- * Detail
-- | 'Alpha' is a class for calculating an floating point value
--  dependent only on the position of a 'Timeline'.
--
-- A 'Alpha' binds a 'Timeline' to a progress function which
-- translates the time T into an adimensional factor alpha. The factor
-- can then be used to drive a 'Behaviour', which will translate the
-- alpha value into something meaningful for a 'Actor'.
--
-- You should provide a 'Timeline' and bind it to the 'Alpha' instance
-- using 'alphaSetTimeline'. You should also set an \"animation
-- mode\", either by using the ClutterAnimationMode values that
-- Clutter itself provides or by registering custom functions using
-- 'alphaRegisterFunc'.
--
-- Instead of a 'AnimationMode' you may provide a function returning
-- the alpha value depending on the progress of the timeline, using
-- 'alphaSetFunc'. The alpha function will be executed each time a new
-- frame in the 'Timeline' is reached.
--
-- Since the alpha function is controlled by the timeline instance,
-- you can pause, stop or resume the 'Alpha' from calling the alpha
-- function by using the appropriate functions of the 'Timeline'
-- object.
--
-- 'Alpha' is used to \"drive\" a Behaviour instance, and it is
-- internally used by the Animation API.
--
-- * Figure 3. Easing modes provided by Clutter
-- <<file:///home/matt/src/clutterhs/doc/easing-modes.png>>
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Alpha'
-- @

-- * Constructors
  alphaNew,
  alphaNewFull,
  alphaNewWithFunc,
-- * Methods

  alphaSetTimeline,
  alphaGetTimeline,

  alphaSetMode,
  alphaGetMode,

  alphaGetAlpha,
  alphaSetFunc,

--alphaSetClosure,
--alphaRegisterClosure,
  alphaRegisterFunc,

-- * Attributes
  alphaTimeline,
  alphaMode,
  alphaAlpha
  ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes


-- | Creates a new ClutterAlpha instance. You must set a function to
--   compute the alpha value using 'alphaSetFunc' and bind a
--   'Timeline' object to the 'Alpha' instance using
--   'alphaSetTimeline'.
--
-- You should use the newly created 'Alpha' instance inside a 'Behaviour' object.
--
-- [@Returns@] the newly created empty 'Alpha' instance.
--
-- * Since 0.2
--
{# fun unsafe alpha_new as ^ { } -> `Alpha' newAlpha* #}


-- | Creates a new 'Alpha' instance and sets the timeline and animation mode.
--
--   See also 'alphaSetTimeline' and 'alphaSetMode'.
--
-- [@timeline@] 'Timeline' timeline
--
-- [@mode@] animation mode
--
-- [@Returns@] the newly created 'Αlpha'
--
-- * Since 1.0
--
{# fun unsafe alpha_new_full as ^
        { withTimeline* `Timeline', cFromEnum `AnimationMode' } -> `Alpha' newAlpha* #}

-- | Creates a new ClutterAlpha instances and sets the timeline and the alpha function.
--
-- This function will not register func as a global alpha function.
--
-- See also alphaSetTimeline and 'alphaSetFunc'.
--
-- * Since 1.0
--
alphaNewWithFunc :: Timeline    -- ^ a 'Timeline'
                 -> AlphaFunc   -- ^ an 'AlphaFunc'
                  -> IO Alpha   -- ^ The newly created 'Alpha'
alphaNewWithFunc tl af = withTimeline tl $ \tlptr -> do
                         afptr <- newAlphaFunc af
                         gdestroy <- mkFunPtrDestroyNotify afptr
                         a <- {# call unsafe alpha_new_with_func #} tlptr afptr nullPtr gdestroy
                         newAlpha a

-- | Binds alpha to timeline.
--
--[@alpha@] An 'Alpha'
--
--[@timeline@] A 'Timeline'
--
-- * Since 0.2
--
{# fun unsafe alpha_set_timeline as ^
       { withAlpha* `Alpha', withTimeline* `Timeline' } -> `()' #}

-- | Gets the 'Timeline' bound to alpha.
--
-- [@alpha@] an 'Alpha'
--
-- [@timeline@] a 'Timeline'
--
-- * Since 0.2
--
{# fun unsafe alpha_get_timeline as ^
       { withAlpha* `Alpha' } -> `Timeline' newTimeline* #}

-- | A 'Timeline' instance used to drive the alpha function
--
-- * Since 0.2
--
alphaTimeline :: Attr Alpha Timeline
alphaTimeline = newAttr alphaGetTimeline alphaSetTimeline

--CHECKME/FIXME: AlphaMode can be something returned from register_func?
--how does enum stuff handle this?

-- | Sets the progress function of alpha using the symbolic value of
--   mode, as taken by the 'AnimationMode' enumeration or using
--   the value returned by 'alphaRegisterFunc'.
--
-- [@alpha@] an 'Alpha'
--
-- [@mode@] an 'AnimationMode'
--
-- * Since 1.0
--
{# fun unsafe alpha_set_mode as ^
       { withAlpha* `Alpha', cFromEnum `AnimationMode' } -> `()' #}

-- | Retrieves the 'AnimationMode' used by an 'Αlpha'.
--
-- * Since 0.2
--
{# fun unsafe alpha_get_mode as ^
       { withAlpha* `Alpha' } -> `AnimationMode' cToEnum #}

--CHECKME: The custom mode return
-- | The progress function logical id - either a value from the
--   'AnimationMode' enumeration or a value returned by
--   'alphaRegisterFunc'.
--
-- If CLUTTER_CUSTOM_MODE is used then the function set using
-- 'alphaSetFunc' will be
-- used.
--
-- * Since 1.0
--
alphaMode :: Attr Alpha AnimationMode
alphaMode = newAttr alphaGetMode alphaSetMode

-- | Query the current alpha value
--
-- * Since 0.2
--
{# fun unsafe alpha_get_alpha as ^ { withAlpha* `Alpha' } -> `Double' #}

-- | The alpha value as computed by the alpha function. The linear
-- interval is 0.0 to 1.0, but the Alpha allows overshooting by one
-- unit in each direction, so the valid interval is -1.0 to 2.0.
-- Allowed values: [-1,2]
--
-- Default value: 0
--
-- * Since 0.2
--
alphaAlpha :: ReadAttr Alpha Double
alphaAlpha = readAttr alphaGetAlpha


-- | Sets the 'AlphaFunc' function used to compute the alpha value at
--   each frame of the 'Timeline' bound to alpha.
--  This function will not register func as a global alpha function.
--
-- * Since 1.0
--
alphaSetFunc :: Alpha -> AlphaFunc -> IO ()
alphaSetFunc alp af = withAlpha alp $ \alpPtr -> do
                        afptr <- newAlphaFunc af
                        gdestroy <- mkFunPtrDestroyNotify afptr
                        {# call unsafe alpha_set_func #} alpPtr afptr nullPtr gdestroy

--TODO: some kind of ID Type for this.
--TODO: What is CLUTTER_ANIMATION_LAST here? some kind of macro
-- | Registers a global alpha function and returns its logical id to
--   be used by 'alphaSetMode' or by 'Animation'.
--
--  The logical id is always greater than CLUTTER_ANIMATION_LAST.
--
-- * Since 1.0
--
alphaRegisterFunc :: AlphaFunc -> IO Word64
alphaRegisterFunc af = do
  afptr <- newAlphaFunc af
  ret <- {# call unsafe alpha_register_func #} afptr nullPtr
  return (cIntConv ret)

--pretty sure don't care about gclosure
--{# fun unsafe alpha_set_closure as ^ { withAlpha* `Alpha', `GClosure' } -> `()' #}
--{# fun unsafe alpha_register_closure as ^ { `GClosure' } -> `GULong' #}

