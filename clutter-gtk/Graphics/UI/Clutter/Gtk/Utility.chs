-- -*-haskell-*-
--  ClutterGtkUtility
--
--  Author : Matthew Arsenault
--
--  Created: 28 Nov 2009
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
#include <clutter-gtk/clutter-gtk.h>

{# context lib="clutter" prefix="gtk" #}

module Graphics.UI.Clutter.Gtk.Utility (
-- * Types
  GtkInitError(..),
  GtkTextureError(..),

-- * Misc.
  gtkClutterInit,
  clutterGetFgColor,
  clutterGetBgColor,
  clutterGetTextColor,
  clutterGetBaseColor,
  clutterGetTextAaColor,
  clutterGetLightColor,
  clutterGetMidColor,
  clutterGetDarkColor,

  clutterTextureNewFromPixbuf,
  clutterTextureNewFromStock,
  clutterTextureNewFromIconName

  ) where

import C2HS
import Control.Monad (when, liftM)
import System.Environment (getArgs, getProgName)

{# import Graphics.UI.Clutter.Gtk.Types #}

import Graphics.UI.Gtk.Types
import Graphics.UI.Clutter.Types
import System.Glib.UTFString
import Graphics.UI.Gtk.General.Enums
import Graphics.UI.Gtk.General.IconFactory (IconSize(..))
import Graphics.UI.Clutter.Types
import Graphics.UI.Clutter.Utility

{# pointer *GtkWidget as WidgetPtr foreign -> Widget nocode #}
{# pointer *ClutterColor as ColorPtr foreign -> Color nocode #}
{# pointer *ClutterActor as ActorPtr foreign -> Actor nocode #}
{# pointer *GdkPixbuf as PixbufPtr foreign -> Pixbuf nocode #}

-- CHECKME: Unicode?
gtkClutterInit :: IO InitError
gtkClutterInit = do
  prog <- getProgName
  args <- getArgs
  let allArgs = (prog:args)
  withMany withUTFString allArgs $ \addrs  ->
    withArrayLen addrs $ \argc argv ->
    with argv $ \argvp ->
    with argc $ \argcp -> do
                res <- liftM cToEnum $ {# call unsafe clutter_init #} (castPtr argcp) (castPtr argvp)
                when (res /= InitSuccess) $ error ("Cannot initialize ClutterGtk." ++ show res)
                return res



{# fun unsafe clutter_get_fg_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_bg_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_text_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_base_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_text_aa_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_light_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_mid_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

{# fun unsafe clutter_get_dark_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

withPixbuf = withForeignPtr . unPixbuf

{# fun unsafe clutter_texture_new_from_pixbuf as ^
    { withPixbuf* `Pixbuf' } -> `Texture' newTexture* #}


{# fun unsafe clutter_texture_new_from_stock as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', `String', cFromEnum `IconSize' } -> `Maybe Texture' maybeNewTexture* #}

{# fun unsafe clutter_texture_new_from_icon_name as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', `String', cFromEnum `IconSize' } -> `Maybe Texture' maybeNewTexture* #}


--TODO: setFromPixbuf, setFromStock, etc.


