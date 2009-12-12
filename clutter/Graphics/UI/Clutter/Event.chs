-- -*-haskell-*-
--  ClutterEvent
--
--  Author : Matthew Arsenault
--
--  Created: 19 Sep 2009
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
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# OPTIONS_HADDOCK prune #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

--TODO: Note the low level stuff ignored here
--TODO: This is in need of a lot of testing

module Graphics.UI.Clutter.Event (
-- * Types
  ModifierType(..),
  ScrollDirection(..),
  InputDeviceType(..),
  EventType(..),
  EventFlags(..),
  StageState(..),
  InputDevice,
  DeviceID,
  Timestamp,

-- * Event monad, and type tags
  EventM,

  EAny,
  EButton,
  EKey,
  EMotion,
  EScroll,
  EStageState,
  ECrossing,

-- * Accessor functions for event information
  eventCoordinates,
  eventState,
  eventTime,
  eventSource,
  eventStage,
  eventFlags,

  eventButton,
  eventClickCount,
  eventKeySymbol,
  eventKeyCode,
  eventKeyUnicode,

  eventRelated,
  eventScrollDirection,

  keysymToUnicode,

  eventDevice,
  eventDeviceId,
  eventDeviceType,

-- * Misc
  eventPut,
  getCurrentEventTime,
  eventsPending,

  getInputDeviceForId,
  inputDeviceGetDeviceId,
  inputDeviceGetDeviceType,

  tryEvent,
  stopEvent,

  eventM    --should be private
  ) where

{# import Graphics.UI.Clutter.Enums #}
{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

--FIXME: Conflict with EventType Nothing
import Prelude hiding (Nothing, catch)
import qualified Prelude as P

import C2HS
import System.Glib.Signals
import System.Glib.Flags
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.List (isPrefixOf)
import Control.Exception (Handler(..),
                          PatternMatchFail(..),
                          catches,
                          throw)
import System.IO.Error (isUserError, ioeGetErrorString)

--TODO: Move keyval

--Taken almost straight from Gtk. Should I try something else?

-- | A monad providing access to data in an event.
--
type EventM t a = ReaderT (Ptr t) IO a

-- | A tag for events that do not carry any event-specific information.
data EAny = EAny

-- | A tag for /Button/ events.
data EButton = EButton

-- | A tag for /key/ events.
data EKey = EKey

-- | A tag for /Motion/ events.
data EMotion = EMotion

-- | A tag for /Scroll/ events.
data EScroll = EScroll

-- | A tag for /StageState/ events.
data EStageState = EStageState

-- | A tag for /Crossing/ events.
data ECrossing = ECrossing


--Clutter seems to not have the event mask stuff
eventM :: ActorClass a => SignalName ->
  ConnectAfter -> a -> (EventM t Bool) -> IO (ConnectId a)
eventM name after obj fun = connect_PTR__BOOL name after obj (runReaderT fun)


-- | Execute an event handler and assume it handled the event unless it
--   threw a pattern match exception.
--TODO: Support Old GHC exceptions here?
--Other exceptions? Why does gdk use pattern?
tryEvent :: EventM any () -> EventM any Bool
tryEvent act = do
  ptr <- ask
  liftIO $ (runReaderT (act >> return True) ptr)
    `catches` [ Handler (\ (PatternMatchFail _) -> return False),
                Handler (\ e -> if isUserError e && "Pattern" `isPrefixOf` ioeGetErrorString e
                                then return False
                                else throw e) ]


-- | Explicitly stop the handling of an event. This function should only be
--   called inside a handler that is wrapped with 'tryEvent'. (It merely
--   throws a bogus pattern matching error which 'tryEvent' interprets as if
--   the handler does not handle the event.)
stopEvent :: EventM any ()
stopEvent =
  liftIO $ throw (PatternMatchFail "EventM.stopEvent called explicitly")


class HasCoordinates a
instance HasCoordinates EButton
instance HasCoordinates EMotion
instance HasCoordinates EScroll
instance HasCoordinates ECrossing

class HasModifierType a
instance HasModifierType EButton
instance HasModifierType EKey
instance HasModifierType EMotion
instance HasModifierType EScroll


--FIXME: Evil casting

-- | Retrieves the 'EventFlags' of event
--
-- the event flags
--
-- * Since 1.0
--
eventFlags :: EventM t [EventFlags]
eventFlags = ask >>= \ptr ->
             liftIO $ liftM cToFlags ({# call unsafe event_get_flags #} (castPtr ptr))


-- | Retrieves the source 'Stage' the event originated for, or
--   @Nothing@ if the event has no stage.
--
-- * Since 0.8
--
eventStage :: EventM t (Maybe Stage)
eventStage = ask >>= \ptr ->
             liftIO $ maybeNewStage . castPtr =<< {# call unsafe event_get_stage #} (castPtr ptr)



-- | Retrieves the source 'Actor' the event originated from, or
--   @Nothing@ if the event has no source.
--
-- * Since 0.6
--
eventSource :: EventM t (Maybe Actor)
eventSource = ask >>= \ptr ->
             liftIO $ maybeNewActor =<< {# call unsafe event_get_source #} (castPtr ptr)



-- | Retrieves the related actor of a crossing event.
--
-- @Just@ the related 'Actor', or @Nothing@. transfer none.
-- * Since 1.0
--
eventRelated :: EventM ECrossing (Maybe Actor)
eventRelated = ask >>= \ptr ->
  liftIO $ maybeNewActor =<< {# call unsafe event_get_related #} (castPtr ptr)



-- | Retrieve the @(x,y)@ coordinates of the mouse.
eventCoordinates :: HasCoordinates t => EventM t (Float, Float)
eventCoordinates = ask >>= \ptr ->
  liftIO $ alloca $ \xptr ->
             alloca $ \yptr -> do
               {# call unsafe event_get_coords #} (castPtr ptr) xptr yptr
               x <- peekFloatConv xptr
               y <- peekFloatConv yptr
               return (x,y)

-- | The time of the event, or CLUTTER_CURRENT_TIME
--
-- * Since 0.4
--
eventTime :: EventM t Timestamp
eventTime = ask >>= \ptr ->
            liftIO $ liftM fromIntegral ({# call unsafe event_get_time #} (castPtr ptr))


-- | Retrieves the modifier state of the event.
--
-- the modifier state parameters, or []
--
-- * Since 0.4
--
eventState :: HasModifierType t => EventM t [ModifierType]
eventState = do
  ptr <- ask
  liftIO $
    liftM (toModFlags . cIntConv) $ {# call unsafe event_get_state #} (castPtr ptr)


-- | Retrieves the button number of event
--
-- the button number
--
-- * Since 1.0
--
eventButton :: EventM EButton Word32
eventButton = ask >>= \ptr ->
              liftIO $ liftM cIntConv ({# call unsafe event_get_button #} (castPtr ptr))



-- | Retrieves the number of clicks of event
--
-- the click count
--
-- * Since 1.0
--
eventClickCount :: EventM EButton Word
eventClickCount = ask >>= \ptr ->
                  liftIO $ liftM cIntConv ({# call unsafe event_get_click_count #} (castPtr ptr))



-- | Retrieves the key symbol of event
--
-- the key symbol representing the key
--
-- * Since 1.0
--
eventKeySymbol :: EventM EKey Word
eventKeySymbol = ask >>= \ptr ->
                 liftIO $ liftM cIntConv ({# call unsafe event_get_key_symbol #} (castPtr ptr))



-- | Retrieves the keycode of the key that caused event
--
-- The keycode representing the key
--
-- * Since 1.0
--
eventKeyCode :: EventM EKey Word16
eventKeyCode = ask >>= \ptr ->
               liftIO $ liftM cIntConv ({# get ClutterKeyEvent->hardware_keycode #} ptr)


-- | Retrieves the unicode value for the key that caused keyev.
--
-- The unicode value representing the key
--
eventKeyUnicode :: EventM EKey Word32
eventKeyUnicode = ask >>= \ptr ->
                 liftIO $ liftM cIntConv ({# get ClutterKeyEvent->unicode_value #} ptr)



-- | Retrieves the direction of the scrolling of event
--
-- the scrolling direction
--
-- * Since 1.0
--
eventScrollDirection :: EventM EScroll ScrollDirection
eventScrollDirection = ask >>= \ptr ->
              liftIO $ liftM cToEnum ({# get ClutterScrollEvent->direction #} ptr)


--TODO: Should this happen automatically? unicode char?
{# fun unsafe keysym_to_unicode as ^ { cIntConv `Word' } -> `Word' cIntConv #}



-- | Retrieves the 'InputDevice' from its id.
--
-- [@id@] a device id
--
-- [@Returns@] @Just@ an 'InputDevice', or @Nothing@. transfer none.
--
-- * Since 0.8
--
getInputDeviceForId :: DeviceID -> IO (Maybe InputDevice)
getInputDeviceForId did = let cdid = cIntConv did
                          in do
                            dev <- {# call unsafe get_input_device_for_id #} cdid
                            return $ if dev == nullPtr
                                       then P.Nothing
                                       else Just (castPtr dev)


-- | Retrieves the unique identifier of device
--
-- [@device@] an 'InputDevice'
--
-- [@Returns@] the identifier of the device
--
-- * Since 1.0
--
{# fun unsafe input_device_get_device_id as ^ { castPtr `InputDevice' } -> `DeviceID' cIntConv #}


-- | Retrieves the type of device
--
-- [@device@] an 'InputDevice'
--
-- [@Returns@] the type of the device
--
-- * Since 1.0
--
{# fun unsafe input_device_get_device_type as ^ { castPtr `InputDevice' } -> `InputDeviceType' cToEnum #}


eventDevice :: EventM t (Maybe InputDevice)
eventDevice = ask >>= \ptr ->
                 liftIO $ do
                   device <- {# call unsafe event_get_device #} (castPtr ptr)
                   return $ if device == nullPtr
                              then P.Nothing
                              else Just (castPtr device)


data InputDeviceStruct = InputDeviceStruct

{# pointer *ClutterInputDevice as InputDevice -> InputDeviceStruct #}


-- | Retrieves the events device id if set.
--
-- @Just@ A unique identifier for the device or @Nothing@ if the event
-- has no specific device set.
--
eventDeviceId :: EventM any (Maybe DeviceID)
eventDeviceId = ask >>= \ptr ->
                liftIO $ do
                  did <- liftM cIntConv $ {# call unsafe event_get_device_id #} (castPtr ptr)
                  return $ if did == (-1)
                             then P.Nothing
                             else Just did


-- | Retrieves the type of the device for event
--
-- the 'InputDeviceType' for the device, if any is set
--
-- * Since 1.0
--
eventDeviceType :: EventM any InputDeviceType
eventDeviceType = ask >>= \ptr ->
                liftIO $ liftM cToEnum $ {# call unsafe event_get_device_type #} (castPtr ptr)


-- | Retrieves the timestamp of the last event, if there is an event
--   or if the event has a timestamp.
--
-- [@Returns@] the event timestamp, or CLUTTER_CURRENT_TIME
--
-- * Since 1.0
--
{# fun unsafe get_current_event_time as ^ { } -> `Timestamp' cIntConv #}



-- | Checks if events are pending in the event queue.
--
-- [@Returns@] @True@ if there are pending events, @False@ otherwise.
--
-- * Since 0.4
--
{# fun unsafe events_pending as ^ { } -> `Bool' #}


--CHECKME: Bad?
-- | Puts a copy of the event on the back of the event queue. The
--   event will have the 'EventFlagSynthetic' flag set. If the source
--   is set event signals will be emitted for this source and
--   capture/bubbling for its ancestors. If the source is not set it
--   will be generated by picking or use the actor that currently has
--   keyboard focus
--
-- * Since 0.6
--
eventPut :: EventM any ()
eventPut = ask >>= \ptr -> liftIO $
                {# call unsafe event_put #} (castPtr ptr)

