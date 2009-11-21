{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ------------
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Created: 19 September 2009
--
--  Copyright (C) 2000-2005 Axel Simon
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
-- #hide

-- These functions are used to connect signals to widgets. They are auto-
-- matically created through HookGenerator.hs which takes a list of possible
-- function signatures that are included in the GTK sources (gtkmarshal.list).
--
-- The object system in the second version of GTK is based on GObject from
-- GLIB. This base class is rather primitive in that it only implements
-- ref and unref methods (and others that are not interesting to us). If
-- the marshall list mentions OBJECT it refers to an instance of this
-- GObject which is automatically wrapped with a ref and unref call.
-- Structures which are not derived from GObject have to be passed as
-- BOXED which gives the signal connect function a possiblity to do the
-- conversion into a proper ForeignPtr type. In special cases the signal
-- connect function use a PTR type which will then be mangled in the
-- user function directly. The latter is needed if a signal delivers a
-- pointer to a string and its length in a separate integer.
--
module Graphics.UI.Clutter.Signals (
  module System.Glib.Signals,

  connect_PTR__BOOL,
  connect_ENUM__BOOL,
  connect_NONE__BOOL,
  connect_PTR__INT,
  connect_BOOL__NONE,
  connect_INT__NONE,
  connect_WORD__NONE,
  connect_NONE__NONE,
  connect_OBJECT__NONE,
  connect_PTR__NONE,
  connect_BOXED__NONE,
  connect_PTR_ENUM__NONE,
  connect_CHAR_INT__NONE,
  connect_STRING_INT__NONE,
  connect_STRING_WORD__NONE,
  connect_INT_INT__NONE,
  
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString (peekUTFString)
import System.Glib.GError (failOnGError)
import System.Glib.Signals
import System.Glib.GObject

{#context lib="clutter" prefix="clutter" #}


-- Here are the generators that turn a Haskell function into
-- a C function pointer. The fist Argument is always the widget,
-- the last one is the user g_pointer. Both are ignored.


connect_PTR__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO Bool) ->
  IO (ConnectId obj)
connect_PTR__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO Bool
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

connect_ENUM__BOOL :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> IO Bool) ->
  IO (ConnectId obj)
connect_ENUM__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO Bool
        action _ enum1 =
          failOnGError $
          user (toEnum enum1)

connect_NONE__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO Bool) ->
  IO (ConnectId obj)
connect_NONE__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO Bool
        action _ =
          failOnGError $
          user

connect_PTR__INT :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO Int) ->
  IO (ConnectId obj)
connect_PTR__INT signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO Int
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

connect_BOOL__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Bool -> IO ()) ->
  IO (ConnectId obj)
connect_BOOL__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Bool -> IO ()
        action _ bool1 =
          failOnGError $
          user bool1

connect_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> IO ()) ->
  IO (ConnectId obj)
connect_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO ()
        action _ int1 =
          failOnGError $
          user int1

connect_WORD__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> IO ()) ->
  IO (ConnectId obj)
connect_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> IO ()
        action _ int1 =
          failOnGError $
          user int1

connect_NONE__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO ()) ->
  IO (ConnectId obj)
connect_NONE__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO ()
        action _ =
          failOnGError $
          user

connect_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 =
          failOnGError $
          makeNewGObject mkGObject (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1')

connect_PTR__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO ()) ->
  IO (ConnectId obj)
connect_PTR__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO ()
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

connect_BOXED__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO ()
        action _ box1 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1'

connect_PTR_ENUM__NONE :: 
  (Enum b, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> b -> IO ()) ->
  IO (ConnectId obj)
connect_PTR_ENUM__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Int -> IO ()
        action _ ptr1 enum2 =
          failOnGError $
          user (castPtr ptr1) (toEnum enum2)

connect_CHAR_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Char -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_CHAR_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Char -> Int -> IO ()
        action _ char1 int2 =
          failOnGError $
          user char1 int2

connect_STRING_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (String -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_STRING_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> Int -> IO ()
        action _ str1 int2 =
          failOnGError $
          peekUTFString str1 >>= \str1' ->
          user str1' int2

connect_STRING_WORD__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (String -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_STRING_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> Word -> IO ()
        action _ str1 int2 =
          failOnGError $
          peekUTFString str1 >>= \str1' ->
          user str1' int2

connect_INT_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_INT_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Int -> IO ()
        action _ int1 int2 =
          failOnGError $
          user int1 int2

