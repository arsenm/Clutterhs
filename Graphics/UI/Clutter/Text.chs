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
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GInitiallyUnowned'
-- |         +----'Actor'
-- |               +----'Text'
-- @

-- * Constructors
  textNew,
  textNewFull,
  textNewWithText,

-- * Methods
  textGetText,
  textSetText,

  textGetActivatable,
  textSetActivatable,

  textSetAttributes,
  textGetAttributes,

  textGetColor,
  textSetColor,

  textGetEllipsize,
  textSetEllipsize,

  textGetFontName,
  textSetFontName,

  textSetPasswordChar,
  textGetPasswordChar,

  textGetJustify,
  textSetJustify,

  textGetLayout,

  textSetLineAlignment,
  textGetLineAlignment,

  textGetLineWrap,
  textSetLineWrap,

  textSetLineWrapMode,
  textGetLineWrapMode,

  textSetMaxLength,
  textGetMaxLength,

  textSetSelectable,
  textGetSelectable,

  textSetSelection,
  textGetSelection,

  textGetSelectionBound,
  textSetSelectionBound,

  textSetSingleLineMode,
  textGetSingleLineMode,

  textSetUseMarkup,
  textGetUseMarkup,

  textSetEditable,
  textGetEditable,

  textInsertText,
  textInsertUnichar,

  textDeleteChars,
  textDeleteText,

  textDeleteSelection,
  textGetChars,

  textGetCursorColor,
  textSetCursorColor,

  textGetSelectionColor,
  textSetSelectionColor,

  textSetCursorPosition,
  textGetCursorPosition,

  textSetCursorVisible,
  textGetCursorVisible,

  textSetCursorSize,
  textGetCursorSize,

  textActivate,
  textPositionToCoords,

#if CLUTTER_CHECK_VERSION(1,2,0)
  textSetPreeditString,
#endif

--TODO: Title for this
-- * Related Types
  --TODO: Export more of Pango?
  PangoLayout,
  LayoutWrapMode,
  LayoutAlignment,
  EllipsizeMode,

-- * Attributes
  textText,
  textActivatable,
--textAttributes,
  textColor,
  textEllipsize,
  textFontName,
  textPasswordChar,
  textJustify,
  textLayout,
  textLineWrap,
  textLineAlignment,
  textLineWrapMode,
  textMaxLength,
  textSelectable,
--textSelection,
  textSelectionBound,
  textSingleLineMode,
  textUseMarkup,
  textEditable,
  textCursorColor,
  textSelectionColor,
  textCursorPosition,
  textCursorVisible,
  textCursorSize,

-- * Signals
  onActivate,
  afterActivate,
  activate,
--onCursorEvent,
--afterCursorEvent,
--cursorEvent,
  onTextChanged,
  afterTextChanged,
  textChanged
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}

import C2HS
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.UTFString

import Control.Monad (liftM)
import Data.IORef

import Graphics.UI.Gtk.Types (mkPangoLayoutRaw, toPangoLayoutRaw, unPangoLayoutRaw)
import Graphics.UI.Gtk.Pango.Types
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Attributes
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

--CHECKME: Something seems unsafe about this, also stupid get text out for correction
textSetAttributes :: Text -> [PangoAttribute] -> IO ()
textSetAttributes txt pattrs = let func = {# call unsafe text_set_attributes #}
                               in withText txt $ \txtPtr -> do
                                    pStr <- makeNewPangoString =<< textGetText txt
                                    withAttrList pStr pattrs $ \attrPtr ->
                                      func txtPtr attrPtr

--getting text out seems convoluted and avoidable
--also why [[PA]]? and not [PA]?
textGetAttributes :: Text -> IO [[PangoAttribute]]
textGetAttributes text = withText text $ \txtPtr -> do
                           attrPtr <- {# call unsafe text_get_attributes #} txtPtr
                           correct <- liftM genUTFOfs (textGetText text)  --TODO: silly
                           fromAttrList correct attrPtr


--textAttributes :: Attr Text [PangoAttribute]
--textAttributes = newAttr textGetAttributes textSetAttributes


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

#if CLUTTER_CHECK_VERSION(1,2,0)
--CHECKME: I've never used Pango, and not really sure if this is good
--also it seems weird.
textSetPreeditString :: Text -> String -> [PangoAttribute] -> Word -> IO ()
textSetPreeditString text str pattrs cpos = let func = {# call unsafe text_set_preedit_string #}
                                            in withText text $ \txtPtr ->
                                                 withUTFString str $ \strPtr -> do
                                                   pStr <- makeNewPangoString str
                                                   withAttrList pStr pattrs $ \attrPtr ->
                                                     func txtPtr strPtr attrPtr (cIntConv cpos)
#endif

onActivate, afterActivate :: Text -> IO () -> IO (ConnectId Text)
onActivate = connect_NONE__NONE "activate" False
afterActivate = connect_NONE__NONE "activate" True

activate :: Signal Text (IO ())
activate = Signal (connect_NONE__NONE "activate")

--TODO: How to use Geometry here?
{-
onCursorEvent, afterCursorEvent :: Text -> (Geometry -> IO ()) -> IO (ConnectId Text)
onCursorEvent = connect_PTR__NONE "cursor-event" False
afterCursorEvent = connect_PTR__NONE "cursor-event" True
-}

cursorEvent :: Signal Text (IO ())
cursorEvent = Signal (connect_NONE__NONE "cursor-event")

onTextChanged, afterTextChanged :: Text -> IO () -> IO (ConnectId Text)
onTextChanged = connect_NONE__NONE "text-changed" False
afterTextChanged = connect_NONE__NONE "text-changed" True

textChanged :: Signal Text (IO ())
textChanged = Signal (connect_NONE__NONE "text-changed")

