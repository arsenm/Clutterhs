-- -*-haskell-*-
--  Clutter Text
--
--  Author : Matthew Arsenault
--
--  Created: 17 Sep 2009
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
#include <pango/pango.h>

{# context lib="clutter" prefix="clutter" #}
{# context lib="pango" prefix="pango" #}

module Graphics.UI.Clutter.Text (
                                 textNew,
                                 textNewFull,
                                 textNewWithText,

                                 textGetText,
                                 textSetText,
                                 textText,

                                 textGetActivatable,
                                 textSetActivatable,
                                 textActivatable,

                                 textGetColor,
                                 textSetColor,
                                 textColor,

                                 textGetEllipsize,
                                 textSetEllipsize,
                                 textEllipsize,

                                 textGetFontName,
                                 textSetFontName,
                                 textFontName,

                                 textSetPasswordChar,
                                 textGetPasswordChar,
                                 textPasswordChar,

                                 textGetJustify,
                                 textSetJustify,
                                 textJustify,

                                 textGetLayout,
                                 textLayout,

                                 textSetLineAlignment,
                                 textGetLineAlignment,
                                 textLineAlignment,

                                 textGetLineWrap,
                                 textSetLineWrap,
                                 textLineWrap,

                                 textSetLineWrapMode,
                                 textGetLineWrapMode,
                                 textLineWrapMode,

                                 textSetMaxLength,
                                 textGetMaxLength,
                                 textMaxLength,

                                 textSetSelectable,
                                 textGetSelectable,
                                 textSelectable,

                                 textSetSelection,
                                 textGetSelection,
                               --textSelection,

                                 textGetSelectionBound,
                                 textSetSelectionBound,
                                 textSelectionBound,

                                 textSetSingleLineMode,
                                 textGetSingleLineMode,
                                 textSingleLineMode,

                                 textSetUseMarkup,
                                 textGetUseMarkup,
                                 textUseMarkup,

                                 textSetEditable,
                                 textGetEditable,
                                 textEditable,

                                 textInsertText,
                                 textInsertUnichar,

                                 textDeleteChars,
                                 textDeleteText,

                                 textDeleteSelection,
                                 textGetChars,

                                 textGetCursorColor,
                                 textSetCursorColor,
                                 textCursorColor,

                                 textGetSelectionColor,
                                 textSetSelectionColor,
                                 textSelectionColor,

                                 textSetCursorPosition,
                                 textGetCursorPosition,
                                 textCursorPosition,

                                 textSetCursorVisible,
                                 textGetCursorVisible,
                                 textCursorVisible,

                                 textSetCursorSize,
                                 textGetCursorSize,
                                 textCursorSize,

                                 textActivate,
                                 textPositionToCoords,

                                 --TODO: Export more of Pango?
                                 PangoLayout,
                                 LayoutWrapMode,
                                 LayoutAlignment,
                                 EllipsizeMode,
                                ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.UTFString

import Control.Monad (liftM)
import Data.IORef

import Graphics.UI.Gtk.Types (mkPangoLayoutRaw, toPangoLayoutRaw, unPangoLayoutRaw)
import Graphics.UI.Gtk.Pango.Types
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Enums (EllipsizeMode)

--CHECKME: Is LayoutWrapMode/LayoutAlignment the wrap mode we want?


{# fun unsafe text_new as ^ {} -> `Text' newText* #}
{# fun unsafe text_new_full as ^ { `String', `String', withColor* `Color' } -> `Text' newText* #}
{# fun unsafe text_new_with_text as ^ { `String', `String' } -> `Text' newText* #}


{# fun unsafe text_get_text as ^ { withText* `Text' } -> `String' #}
{# fun unsafe text_set_text as ^ { withText* `Text', `String' } -> `()' #}
textText :: Attr Text String
textText = newAttr textGetText textSetText

{# fun unsafe text_get_activatable as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_activatable as ^ { withText* `Text', `Bool' } -> `()' #}
textActivatable :: Attr Text Bool
textActivatable = newAttr textGetActivatable textSetActivatable

{-
{# fun unsafe text_get_attributes as ^ { withText* `Text' } -> `PangoAttrList' #}
{# fun unsafe text_set_attributes as ^ { withText* `Text', `PangoAttrList' } -> `()' #}
textAttributes :: Attr Text PangoAttrList
textAttributes = newAttr textGetAttributes textSetAttributes
-}

{# fun unsafe text_get_color as ^ {withText* `Text', alloca- `Color' peek* } -> `()' #}
{# fun unsafe text_set_color as ^ {withText* `Text', withColor* `Color' } -> `()' #}
textColor :: Attr Text Color
textColor = newAttr textGetColor textSetColor


{# fun unsafe text_get_ellipsize as ^ {withText* `Text' } -> `EllipsizeMode' cToEnum #}
{# fun unsafe text_set_ellipsize as ^ {withText* `Text', cFromEnum `EllipsizeMode' } -> `()' #}
textEllipsize :: Attr Text EllipsizeMode
textEllipsize = newAttr textGetEllipsize textSetEllipsize


{# fun unsafe text_get_font_name as ^ { withText* `Text' } -> `String' #}
{# fun unsafe text_set_font_name as ^ { withText* `Text', `String' } -> `()' #}
textFontName :: Attr Text String
textFontName = newAttr textGetFontName textSetFontName

--TODO: Do something with unicode stuff?
{# fun unsafe text_get_password_char as ^ {withText* `Text' } -> `GUnichar' cIntConv #}
{# fun unsafe text_set_password_char as ^ {withText* `Text', cIntConv `GUnichar' } -> `()' #}
textPasswordChar :: Attr Text GUnichar
textPasswordChar = newAttr textGetPasswordChar textSetPasswordChar


{# fun unsafe text_get_justify as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_justify as ^ { withText* `Text', `Bool'} -> `()' #}
textJustify :: Attr Text Bool
textJustify = newAttr textGetJustify textSetJustify

withPangoLayoutRaw = withForeignPtr . unPangoLayoutRaw
--CHECKME: I have no idea if this is right or makes sense.
--particularly the getting the text from the layoutraw and putting it
--in PangoLayout, the newGObject followed by the with, and really just
--this whole function needs to be looked at
textGetLayout :: Text -> IO PangoLayout
textGetLayout self = withText self $ \ctextptr -> do
                       pl <- constructNewGObject mkPangoLayoutRaw $ liftM castPtr $ {# call unsafe text_get_layout #} ctextptr
                       withPangoLayoutRaw pl $ \plptr -> do
                                                    str <- {#call unsafe layout_get_text#} (castPtr plptr) >>= peekUTFString
                                                    ps <- makeNewPangoString str
                                                    psRef <- newIORef ps
                                                    return (PangoLayout psRef pl)

textLayout :: ReadAttr Text PangoLayout
textLayout = readAttr textGetLayout

{# fun unsafe text_get_line_alignment as ^ { withText* `Text' } -> `LayoutAlignment' cToEnum #}
{# fun unsafe text_set_line_alignment as ^ { withText* `Text', cFromEnum `LayoutAlignment'} -> `()' #}
textLineAlignment :: Attr Text LayoutAlignment
textLineAlignment = newAttr textGetLineAlignment textSetLineAlignment


{# fun unsafe text_get_line_wrap as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_line_wrap as ^ { withText* `Text', `Bool'} -> `()' #}
textLineWrap :: Attr Text Bool
textLineWrap = newAttr textGetLineWrap textSetLineWrap


{# fun unsafe text_get_line_wrap_mode as ^ { withText* `Text' } -> `LayoutWrapMode' cToEnum #}
{# fun unsafe text_set_line_wrap_mode as ^ { withText* `Text', cFromEnum `LayoutWrapMode' } -> `()' #}
textLineWrapMode :: Attr Text LayoutWrapMode
textLineWrapMode = newAttr textGetLineWrapMode textSetLineWrapMode


{# fun unsafe text_get_max_length as ^ { withText* `Text' } -> `Int' #}
{# fun unsafe text_set_max_length as ^ { withText* `Text', `Int'} -> `()' #}
textMaxLength :: Attr Text Int
textMaxLength = newAttr textGetMaxLength textSetMaxLength

{# fun unsafe text_get_selectable as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_selectable as ^ { withText* `Text', `Bool'} -> `()' #}
textSelectable :: Attr Text Bool
textSelectable = newAttr textGetSelectable textSetSelectable


{# fun unsafe text_get_selection as ^ { withText* `Text' } -> `String' #}
{# fun unsafe text_set_selection as ^ { withText* `Text', cIntConv `GSSize', cIntConv `GSSize' } -> `()' #}
--this won't work
--textSelection :: Attr Text Int
--textSelection = newAttr textGetSelection textSetSelection


{# fun unsafe text_get_selection_bound as ^ { withText* `Text' } -> `Int' #}
{# fun unsafe text_set_selection_bound as ^ { withText* `Text', `Int' } -> `()' #}
textSelectionBound :: Attr Text Int
textSelectionBound = newAttr textGetSelectionBound textSetSelectionBound


{# fun unsafe text_get_single_line_mode as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_single_line_mode as ^ { withText* `Text', `Bool' } -> `()' #}
textSingleLineMode :: Attr Text Bool
textSingleLineMode = newAttr textGetSingleLineMode textSetSingleLineMode


{# fun unsafe text_get_use_markup as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_use_markup as ^ { withText* `Text', `Bool' } -> `()' #}
textUseMarkup :: Attr Text Bool
textUseMarkup = newAttr textGetUseMarkup textSetUseMarkup

{# fun unsafe text_get_editable as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_editable as ^ { withText* `Text', `Bool' } -> `()' #}
textEditable :: Attr Text Bool
textEditable = newAttr textGetEditable textSetEditable

--Insertions

{# fun unsafe text_insert_text as ^ { withText* `Text', `String', cIntConv `GSSize' } -> `()' #}

{# fun unsafe text_insert_unichar as ^ { withText* `Text', cIntConv `GUnichar' } -> `()' #}
{# fun unsafe text_delete_chars as ^ { withText* `Text', cIntConv `Word' } -> `()' #}

{# fun unsafe text_delete_text as ^ { withText* `Text', cIntConv `GSSize', cIntConv `GSSize' } -> `()' #}

{# fun unsafe text_delete_selection as ^ { withText* `Text' } -> `()' #}
{# fun unsafe text_get_chars as ^ { withText* `Text', cIntConv `GSSize', cIntConv `GSSize' } -> `()' #}

{# fun unsafe text_get_cursor_color as ^ { withText* `Text', alloca- `Color' peek* } -> `()' #}
{# fun unsafe text_set_cursor_color as ^ { withText* `Text', withColor* `Color' } -> `()' #}
textCursorColor :: Attr Text Color
textCursorColor = newAttr textGetCursorColor textSetCursorColor

{# fun unsafe text_get_selection_color as ^ { withText* `Text', alloca- `Color' peek* } -> `()' #}
{# fun unsafe text_set_selection_color as ^ { withText* `Text', withColor* `Color' } -> `()' #}
textSelectionColor :: Attr Text Color
textSelectionColor = newAttr textGetSelectionColor textSetSelectionColor


{# fun unsafe text_get_cursor_position as ^ { withText* `Text' } -> `Int' #}
{# fun unsafe text_set_cursor_position as ^ { withText* `Text', `Int' } -> `()' #}
textCursorPosition :: Attr Text Int
textCursorPosition = newAttr textGetCursorPosition textSetCursorPosition

{# fun unsafe text_get_cursor_visible as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_cursor_visible as ^ { withText* `Text', `Bool' } -> `()' #}
textCursorVisible :: Attr Text Bool
textCursorVisible = newAttr textGetCursorVisible textSetCursorVisible

{# fun unsafe text_get_cursor_size as ^ { withText* `Text' } -> `Int' #}
{# fun unsafe text_set_cursor_size as ^ { withText* `Text', `Int' } -> `()' #}
textCursorSize :: Attr Text Int
textCursorSize = newAttr textGetCursorSize textSetCursorSize

{# fun unsafe text_activate as ^ { withText* `Text' } -> `Bool' #}

{# fun unsafe text_position_to_coords as ^
       { withText* `Text',
         `Int',
         alloca- `Float' peekFloatConv*,
         alloca- `Float' peekFloatConv*,
         alloca- `Float' peekFloatConv*} -> `Bool' #}

