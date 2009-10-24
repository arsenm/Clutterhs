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

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

--TODO: Note the low level stuff ignored here
--TODO: This is in need of a lot of testing
--TODO: Make stuff here more consistent with itself,
--struct using vs. functions. Don't need to do silly struct things.

module Graphics.UI.Clutter.Event (
                                  EventM,

                                  EAny,
                                  EButton,
                                  EKey,
                                  EMotion,
                                  EScroll,
                                  EStageState,
                                  ECrossing,

                                  eventM,
                                  tryEvent,

                                  eventCoordinates,
                                  eventState,
                                  eventTime,
                                  eventSource,
                                  eventStage,
                                  eventFlags,
                                  eventButton,
                                  eventClickCount,
                                  eventKeySymbol,
                                  eventKeyUnicode,

                                  keysymToUnicode,

                                --eventDevice,
                                  eventDeviceId,
                                  eventDeviceType,
                                --eventGetInputDeviceForId,
                                --inputDeviceGetDeviceType,

                                  getCurrentEventTime,

                                  scrollEvent,
                                  motionNotifyEvent,

                                  buttonPressEvent,
                                  buttonReleaseEvent

                                 ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}

--FIXME: Conflict with EventType Nothing
import Prelude hiding (Nothing, catch)
import Data.Maybe (catMaybes)
import qualified Prelude as P

import C2HS
import System.Glib.GObject
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


--FIXME: Move this. guint32
type TimeStamp = Word32

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

eventFlags :: EventM t [EventFlags]
eventFlags = ask >>= \ptr ->
             liftIO $ liftM (toFlags . cIntConv) ({# get ClutterAnyEvent->flags #} ptr)

eventStage :: EventM t Stage
eventStage = ask >>= \ptr ->
             liftIO $ newStage . castPtr =<< {# get ClutterAnyEvent->stage #} ptr

eventSource :: EventM t Actor
eventSource = ask >>= \ptr ->
             liftIO $ newActor =<< {# get ClutterAnyEvent->source #} ptr


-- | Retrieve the @(x,y)@ coordinates of the mouse.
eventCoordinates :: HasCoordinates t => EventM t (Float, Float)
eventCoordinates = do
  ptr <- ask
  liftIO $ do
    ty <- {# get ClutterEvent->type #} ptr
    case cToEnum ty of
      ButtonPress -> do
        x <- {# get ClutterButtonEvent->x #} ptr
        y <- {# get ClutterButtonEvent->y #} ptr
        return (realToFrac x, realToFrac y)
      Motion -> do
        x <- {# get ClutterMotionEvent->x #} ptr
        y <- {# get ClutterMotionEvent->y #} ptr
        return (realToFrac x, realToFrac y)
      Scroll -> do
        x <- {# get ClutterScrollEvent->x #} ptr
        y <- {# get ClutterScrollEvent->y #} ptr
        return (realToFrac x, realToFrac y)
      Enter -> do
        x <- {# get ClutterCrossingEvent->x #} ptr
        y <- {# get ClutterCrossingEvent->y #} ptr
        return (realToFrac x, realToFrac y)
      Leave -> do
        x <- {# get ClutterCrossingEvent->x #} ptr
        y <- {# get ClutterCrossingEvent->y #} ptr
        return (realToFrac x, realToFrac y)
      _ -> error ("eventCoordinates: none for event type " ++ show ty)


eventTime :: EventM t TimeStamp
eventTime = ask >>= \ptr ->
            liftIO $ liftM fromIntegral ({# get ClutterAnyEvent-> time #} ptr)

buttonPressEvent :: ActorClass self => Signal self (EventM EButton Bool)
buttonPressEvent = Signal (eventM "button_press_event")

buttonReleaseEvent :: ActorClass self => Signal self (EventM EButton Bool)
buttonReleaseEvent = Signal (eventM "button_release_event")

scrollEvent :: ActorClass self => Signal self (EventM EScroll Bool)
scrollEvent = Signal (eventM "scroll_event")

motionNotifyEvent :: ActorClass self => Signal self (EventM EMotion Bool)
motionNotifyEvent = Signal (eventM "motion_event")


--The normal one from gtk2hs does not work here. there is a
--discontinuity in the enum of unused bits and also an internally used
--bit, therefore minBound .. maxBound fails, so do this shitty listing
--of all options
toModFlags :: Int -> [ModifierType]
toModFlags n = catMaybes [ if n .&. fromEnum flag == fromEnum flag
                            then Just flag
                            else P.Nothing
                          | flag <- [ShiftMask,
                                     LockMask,
                                     ControlMask,
                                     Mod1Mask,
                                     Mod2Mask,
                                     Mod3Mask,
                                     Mod4Mask,
                                     Mod5Mask,
                                     Button1Mask,
                                     Button2Mask,
                                     Button3Mask,
                                     Button4Mask,
                                     Button5Mask,
                                     SuperMask,
                                     HyperMask,
                                     MetaMask,
                                     ReleaseMask,
                                     ModifierMask]
                         ]

eventState :: HasModifierType t => EventM t [ModifierType]
eventState = do
  ptr <- ask
  liftIO $ do
    ty <- {# get ClutterEvent->type #} ptr
    case cToEnum ty of
      KeyPress -> liftM (toModFlags.cIntConv) ({# get ClutterKeyEvent->modifier_state #} ptr)
      ButtonPress -> liftM (toModFlags.cIntConv) ({# get ClutterButtonEvent->modifier_state #} ptr)
      Motion -> liftM (toModFlags.cIntConv) ({# get ClutterMotionEvent->modifier_state #} ptr)
      Scroll -> liftM (toModFlags.cIntConv) ({# get ClutterScrollEvent->modifier_state #} ptr)
      _ -> error ("eventModifierType: none for event type " ++ show ty)

--TODO: Word32, guint32 i'm sure doesn't matter but whatever
eventButton :: EventM EButton Word32
eventButton = ask >>= \ptr ->
              liftIO $ liftM cIntConv ({# get ClutterButtonEvent->button #} ptr)

eventClickCount :: EventM EButton Word32
eventClickCount = ask >>= \ptr ->
                  liftIO $ liftM cIntConv ({# get ClutterButtonEvent->click_count #} ptr)


--TODO: guint vs. Word32, also conversion
eventKeySymbol :: EventM EButton Word32
eventKeySymbol = ask >>= \ptr ->
                 liftIO $ liftM cIntConv ({# get ClutterKeyEvent->keyval #} ptr)

eventKeyCode :: EventM EButton Word16
eventKeyCode = ask >>= \ptr ->
               liftIO $ liftM cIntConv ({# get ClutterKeyEvent->hardware_keycode #} ptr)

eventKeyUnicode :: EventM EButton Word32
eventKeyUnicode = ask >>= \ptr ->
                 liftIO $ liftM cIntConv ({# get ClutterKeyEvent->unicode_value #} ptr)


eventScrollDirection :: EventM EScroll ScrollDirection
eventScrollDirection = ask >>= \ptr ->
              liftIO $ liftM cToEnum ({# get ClutterScrollEvent->direction #} ptr)

{-
--I don't understand why GDK is doing modif .&. mask stuff
eM allModifs = do
  let mask | allModifs = -1
           | otherwise = defModMask
  ptr <- ask
  liftIO $ do
    (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
    if ty `elem` [ #{const GDK_KEY_PRESS},
                   #{const GDK_KEY_RELEASE}] then do
        (modif ::#gtk2hs_type guint)	<- #{peek GdkEventKey, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_BUTTON_PRESS},
                   #{const GDK_2BUTTON_PRESS},
                   #{const GDK_3BUTTON_PRESS},
                   #{const GDK_BUTTON_RELEASE}] then do
        (modif ::#gtk2hs_type guint)	<- #{peek GdkEventButton, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_SCROLL} ] then do
        (modif ::#gtk2hs_type guint)	<- #{peek GdkEventScroll, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] then do
        (modif ::#gtk2hs_type guint)	<- #{peek GdkEventMotion, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                          #{const GDK_LEAVE_NOTIFY}] then do
        (modif ::#gtk2hs_type guint)	<- #{peek GdkEventCrossing, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else error ("eventModifiers: none for event type "++show ty)
-}

--TODO: Should this happen automatically?. Also guint
{# fun unsafe keysym_to_unicode as ^ { `Int' } -> `Word32' #}

{-
{# fun unsafe get_input_device_for_id as ^ { `Int' } -> `InputDevice' #}

TODO: The Device stuff I don't think is useful without first looking
at some of the backend specific stuff

eventDevice :: EventM t InputDevice
eventDevice = ask >>= \ptr ->
                 liftIO $ liftM cIntConv ({# get ClutterAnyEvent->device #} ptr)

-}
--TODO: DeviceID type? Silly structiness?
--I also don't understand why the cast is needed
eventDeviceId :: EventM any Int
eventDeviceId = ask >>= \ptr ->
                liftIO $ liftM cIntConv $ {# call unsafe event_get_device_id #} (castPtr ptr)


eventDeviceType :: EventM any InputDeviceType
eventDeviceType = ask >>= \ptr ->
                liftIO $ liftM cToEnum $ {# call unsafe event_get_device_type #} (castPtr ptr)

--TODO: Time type for guint32
{# fun unsafe get_current_event_time as ^ { } -> `Word32' #}


