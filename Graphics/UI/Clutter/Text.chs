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

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Text (
                                 textNew,
--                                 textNewFull,
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

                               --textGetEllipsize,
                               --textSetEllipsize,
                               --textEllipsize,

                                 textGetFontName,
                                 textSetFontName,
                                 textFontName,

                               --textSetPasswordChar,
                               --textGetPasswordChar,
                               --textPasswordChar,

                                 textGetJustify,
                                 textSetJustify,
                                 textJustify,

                               --textGetLayout,

                               --testSetLineAlignment,
                               --textGetLineAlignment,
                               --textLineAlignment,

                                 textGetLineWrap,
                                 textSetLineWrap,
                                 textLineWrap,

                               --textSetLineWrapMode,
                               --textGetLineWrapMode,
                               --textLineWrapMode,

                                 textSetMaxLength,
                                 textGetMaxLength,
                                 textMaxLength,

                                 textSetSelectable,
                                 textGetSelectable,
                                 textSelectable,

                               --textSetSelection,
                               --textGetSelection,
                               --textSelection,

                               --textGetSelectionBound,
                               --textSetSelectionBound,
                               --textSelectionBound,

                               --textSetSingleLineMode,
                               --textGetSingleLineMode,
                               --textSingleLineMode,

                               --textSetUseMarkup,
                               --textGetUseMarkup,
                               --textUseMarkup,

                                 textSetEditable,
                                 textGetEditable,
                                 textEditable,

                               --textInsertText,
                               --textInsertUnichar,

                               --textDeleteChars,
                               --textDeleteText,

                               --textDeleteSelection,
                               --textGetChars,

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
                                 textPositionToCoords

                                ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes
import Control.Monad (liftM)

{# fun unsafe text_new as ^ {} -> `Text' newText* #}
{#fun unsafe text_new_with_text as ^ { `String', `String' } -> `Text' newText* #}

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
{# fun unsafe text_set_color as ^ {withText* `Text', withColor* `Color'} -> `()' #}
textColor :: Attr Text Color
textColor = newAttr textGetColor textSetColor

{-
{# fun unsafe text_get_ellipsize as ^ {withText* `Text', alloca- `PangoEllipsizeMode' peek* } -> `()' #}
{# fun unsafe text_set_ellipsize as ^ {withText* `Text', withColor* `PangoEllipsizeMode'} -> `()' #}
textEllipsize :: Attr Text Ellipsize
textEllipsize = newAttr textGetEllipsize textSetEllipsize
-}

{# fun unsafe text_get_font_name as ^ { withText* `Text' } -> `String' #}
{# fun unsafe text_set_font_name as ^ { withText* `Text', `String'} -> `()' #}
textFontName :: Attr Text String
textFontName = newAttr textGetFontName textSetFontName

{-
--TODO: Some kind of unicode stuff
{# fun unsafe text_get_password_char as ^ {withText* `Text' } -> `??' #}
{# fun unsafe text_set_password_char as ^ {withText* `Text', `??' } -> `()' #}
textFontName :: Attr Text ??
textFontName = newAttr textGetPasswordChar textSetPasswordChar
-}

{# fun unsafe text_get_justify as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_justify as ^ { withText* `Text', `Bool'} -> `()' #}
textJustify :: Attr Text Bool
textJustify = newAttr textGetJustify textSetJustify

{-
TODO: Read only attribute??
{# fun unsafe text_get_layout as ^ {withText* `Text' } -> `PangoLayout' #}
textLayout :: Attr Text PangoLayout
textLayout = newAttr textGetJustify textSetJustify


{# fun unsafe text_get_line_alignment as ^ { withText* `Text' } -> `PangoAlignment' #}
{# fun unsafe text_set_line_alignment as ^ { withText* `Text', `PangoAlignment'} -> `()' #}
textAlignment :: Attr Text PangoAlignment
textAlignment = newAttr textGetAlignment textSetAlignment
-}

{# fun unsafe text_get_line_wrap as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_line_wrap as ^ { withText* `Text', `Bool'} -> `()' #}
textLineWrap :: Attr Text Bool
textLineWrap = newAttr textGetLineWrap textSetLineWrap

{-
{# fun unsafe text_get_line_wrap_mode as ^ { withText* `Text' } -> `PangoWrapMode' #}
{# fun unsafe text_set_line_wrap_mode as ^ { withText* `Text', `PangoWrapMode'} -> `()' #}
textLineWrapMode :: Attr Text PangoWrapMode
textLineWrapMode = newAttr textGetLineWrapMode textSetLineWrapMode
-}

{# fun unsafe text_get_max_length as ^ { withText* `Text' } -> `Int' #}
{# fun unsafe text_set_max_length as ^ { withText* `Text', `Int'} -> `()' #}
textMaxLength :: Attr Text Int
textMaxLength = newAttr textGetMaxLength textSetMaxLength

{# fun unsafe text_get_selectable as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_selectable as ^ { withText* `Text', `Bool'} -> `()' #}
textSelectable :: Attr Text Bool
textSelectable = newAttr textGetSelectable textSetSelectable

--TODO: Selection bounds and other functions

{# fun unsafe text_get_editable as ^ { withText* `Text' } -> `Bool' #}
{# fun unsafe text_set_editable as ^ { withText* `Text', `Bool' } -> `()' #}
textEditable :: Attr Text Bool
textEditable = newAttr textGetEditable textSetEditable

--Insertions

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

