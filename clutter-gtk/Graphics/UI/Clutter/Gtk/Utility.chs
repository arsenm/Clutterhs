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
  clutterTextureNewFromIconName,

  textureSetFromPixbuf,
  textureSetFromStock,
  textureSetFromIconName
  ) where

import C2HS
import Control.Monad (when, liftM)
import System.Environment (getArgs, getProgName)

{# import Graphics.UI.Clutter.Gtk.Types #}

import Graphics.UI.Gtk.Types
import Graphics.UI.Clutter.Types
import Graphics.UI.Clutter.Enums
import System.Glib.UTFString
import System.Glib.GError
import Graphics.UI.Gtk.General.Enums
import Graphics.UI.Gtk.General.IconFactory (IconSize(..))
import Graphics.UI.Clutter.Types
import Graphics.UI.Clutter.Utility

{# pointer *GtkWidget as WidgetPtr foreign -> Widget nocode #}
{# pointer *ClutterColor as ColorPtr foreign -> Color nocode #}
{# pointer *ClutterActor as ActorPtr foreign -> Actor nocode #}
{# pointer *ClutterTexture as TexturePtr foreign -> Texture nocode #}
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

--TODO: init_with_args



-- | Retrieves the foreground color of widget for the given state
--
-- [@widget@] a 'Widget'
--
-- [@state@] a state
---
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_fg_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}


-- | Retrieves the background color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_bg_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}


-- | Retrieves the text color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_text_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}



-- | Retrieves the base color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_base_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}


-- | Retrieves the text-aa color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_text_aa_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}


-- | Retrieves the light color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_light_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}


-- | Retrieves the mid color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_mid_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}


-- | Retrieves the dark color of widget for the given state
--
-- [@widget@] a Widget
--
-- [@state@] a state
--
-- [@Returns@] a 'Color'
--
-- * Since 0.8
--
{# fun unsafe clutter_get_dark_color as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', cFromEnum `StateType', alloca- `Color' peek* } -> `()' #}

withPixbuf = withForeignPtr . unPixbuf



-- | Creates a new 'Texture' and sets its contents with a copy of
-- pixbuf.
--
-- [@pixbuf@] a 'Pixbuf'
--
-- [@Returns@] the newly created 'Texture'
--
-- * Since 0.8
--
{# fun unsafe clutter_texture_new_from_pixbuf as ^
    { withPixbuf* `Pixbuf' } -> `Texture' newTexture* #}


-- | Creates a new 'Texture' and sets its contents using the stock
-- icon stock_id as rendered by widget.
--
-- [@widget@] a Widget
--
-- [@stock_id@] the stock id of the icon
--
-- [@size@] the size of the icon, or -1
--
-- [@Returns@] the newly created 'Texture'
--
-- * Since 0.8
--
{# fun unsafe clutter_texture_new_from_stock as ^ `(WidgetClass widget)' =>
    { withWidgetClass* `widget', `String', cFromEnum `IconSize' } -> `Maybe Texture' maybeNewTexture* #}





-- | Creates a new 'Texture' and sets its contents to be the icon_name
-- from the current icon theme.
--
-- [@widget@] @Just@ a Widget or @Nothing@
--
-- [@icon_name@] the name of the icon
--
-- [@size@] the size of the icon, or -1
--
-- [@Returns@] @Just@ the newly created texture, or @Nothing@ if
-- @widget@ was @Nothing@ and icon_name was not found.
--
-- * Since 0.8
--
{# fun unsafe clutter_texture_new_from_icon_name as ^ `(WidgetClass widget)' =>
    { withMaybeWidgetClass* `Maybe widget', `String', cFromEnum `IconSize' } -> `Maybe Texture' maybeNewTexture* #}



-- CHECKME: Is the returned bool really useful here?

-- | Sets the contents of texture with a copy of pixbuf. Throws an
-- exception in event of an error.
--
-- [@texture@] a Texture
--
-- [@pixbuf@] a Pixbuf
--
-- [@Returns@] @True@ on success, @False@ on failure.
--
-- * Since 0.8
--
textureSetFromPixbuf :: (TextureClass texture) => texture -> Pixbuf -> IO Bool
textureSetFromPixbuf texture pixbuf = let func = {# call unsafe clutter_texture_set_from_pixbuf #}
                                      in propagateGError $ \gerrorPtr ->
                                           withTextureClass texture $ \texPtr ->
                                             withPixbuf pixbuf $ \pbPtr ->
                                               liftM cToBool (func texPtr pbPtr gerrorPtr)



-- | Sets the contents of texture using the stock icon stock_id, as
-- rendered by widget. Throws an exception in event of an error.
--
-- [@texture@] a Texture
--
-- [@widget@] a Widget
--
-- [@stock_id@] the stock id of the icon
--
-- [@size@] the size of the icon, or -1
--
-- [@Returns@] @True@ on success, @False@ on failure.
--
-- * Since 0.8
--
textureSetFromStock :: (TextureClass texture, WidgetClass widget) => texture -> widget -> String -> IconSize -> IO Bool
textureSetFromStock texture widget str size = let func = {# call unsafe clutter_texture_set_from_stock #}
                                                  csize = cFromEnum size
                                              in propagateGError $ \gerrorPtr ->
                                                   withTextureClass texture $ \texPtr ->
                                                     withWidgetClass widget $ \widPtr ->
                                                       withCString str $ \strPtr ->
                                                         liftM cToBool (func texPtr widPtr strPtr csize gerrorPtr)




-- | Sets the contents of texture using the icon_name from the current
-- icon theme. Throws an exception in event of an error.
--
-- [@texture@] a Texture
--
-- [@widget@] @Just@ a Widget or @Nothing@
--
-- [@icon_name@] the name of the icon
--
-- [@size@] the icon size or -1
--
-- [@Returns@] @True@ on success, @False@ on failure.
--
-- * Since 0.8
--
textureSetFromIconName :: (TextureClass texture, WidgetClass widget) => texture -> Maybe widget -> String -> IconSize -> IO Bool
textureSetFromIconName texture widget str size = let func = {# call unsafe clutter_texture_set_from_icon_name #}
                                                     csize = cFromEnum size
                                                 in propagateGError $ \gerrorPtr ->
                                                      withTextureClass texture $ \texPtr ->
                                                        withMaybeWidgetClass widget $ \widPtr ->
                                                          withCString str $ \strPtr ->
                                                           liftM cToBool (func texPtr widPtr strPtr csize gerrorPtr)

