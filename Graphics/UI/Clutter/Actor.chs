-- -*-haskell-*-
--  Clutter Color
--
--  Author : Matthew Arsenault
--
--  Created: 10 Sep 2009
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

--TODO: Maybe for the parent property
--TODO: list of the flags and use that

module Graphics.UI.Clutter.Actor (
                                  actorSetFlags,
                                  actorUnsetFlags,
                                  actorGetFlags,
                                  actorShow,
                                  actorShowAll,
                                  actorHide,
                                  actorHideAll,
                                  actorRealize,
                                  actorUnrealize,
                                  actorPaint,
                                  actorQueueRedraw,
                                  actorQueueRelayout,

                                  actorSetPosition,
                                  actorGetPosition,
                                --actorPosition ???? TODO: Tuple Point type???
                                  actorSetSize,
                                  actorGetSize,

                                  actorSetWidth,
                                  actorGetWidth,
                                  actorWidth,
                                  actorSetHeight,
                                  actorGetHeight,
                                  actorHeight,

                                  actorSetX,
                                  actorGetX,
                                  actorX,

                                  actorSetY,
                                  actorGetY,
                                  actorY,

                                  actorMoveBy,
                                  actorSetRotation,
                                  actorSetZRotationFromGravity,
                                  actorGetZRotationGravity,
                                  actorGetRotation,
                                  actorIsRotated,
                                  actorSetOpacity,
                                  actorGetOpacity,
                                  actorOpacity,
                                  actorSetName,
                                  actorGetName,
                                  actorName,
                                  actorGetGid,
                                  actorGid,
                                  actorSetClip,
                                  actorRemoveClip,
                                  actorHasClip,
                                  actorGetClip,

                                  actorSetParent,
                                --actorGetParent,
                                --actorParent
                                  actorReparent,
                                  actorRaise,
                                  actorLower,
                                  actorRaiseTop,
                                  actorLowerBottom,
                                --actorGetStage,
                                  actorSetDepth,
                                  actorGetDepth,
                                  actorDepth,
                                  actorSetScale,
                                  actorSetScaleFull,
                                  actorSetScaleWithGravity,
                                  actorGetScale,
                                  actorGetScaleCenter,
                                  actorGetScaleGravity,
                                  actorIsScaled,
                                --actorApplyTransformToPoint,
                                --actorTransformStagePoint,
                                --actorApplyRelativeTransformToPoint,
                                --actorGetTransformedPosition,
                                --actorGetTransformedSize,
                                --actorGetPaintOpacity,
                                --actorGetPaintVisibility,
                                --actorGetAbsAllocationVertices,
                                --actorGetTransformationMatrix
                                --actorSetAnchorPoint,
                                --actorGetAnchorPoint,
                                --actorSetAnchorPointFromGravity,
                                --actorGetAnchorPointGravity,
                                --actorMoveAnchorPoint,
                                --actorMoveAnchorPointFromGravity,
                                --actorSetReactive,
                                --actorGetReactive,
                                --actorReactive,
                                --actorSetShader,
                                --actorGetShader,
                                --actorShader,
                                --actorSetShaderParam,
                                --actorSetShaderParamFloat,
                                --actorSetShaderParamInt,
                                --actorGrabKeyFocus,
                                --actorGetPangoContext,
                                --actorCreatePangoContext,
                                --actorCreatePangoLayout,
                                  actorIsInClonePaint,

                                --actorBoxNew,
                                --actorBoxCopy,
                                --actorBoxFree,
                                --actorBoxEqual,
                                --actorBoxGetX,
                                --actorBoxGetY,
                                --actorBoxGetWidth,
                                --actorBoxGetHeight,
                                --actorBoxGetOrigin,
                                --actorBoxGetSize,
                                --actorBoxGetArea,
                                --actorBoxContains,
                                --actorBoxFromVertices,
                                --vertexNew,
                                --vertexCopy,
                                --vertexFree,
                                --vertexEqual

                                --signals etc.
                                  onButtonPressEvent,

                                  onShow,
                                  afterShow,
                                  show,

                                  onHide,
                                  afterHide,
                                  hide,

                                  onRealize,
                                  afterRealize,
                                  realize,

                                  onPaint,
                                  afterPaint,
                                  paint,

                                  onKeyFocusIn,
                                  afterKeyFocusIn,
                                  keyFocusIn,

                                  onKeyFocusOut,
                                  afterKeyFocusOut,
                                  keyFocusOut,

                                  onDestroy,
                                  afterDestroy,
                                  destroy

                                 ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}


--FIXME: should I do something about clutter/prelude conflicts?
import Prelude hiding (show)

import C2HS
import Foreign
import Foreign.Ptr
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.Signals

--FIXME: A lot of these need to be marked as safe, not unsafe for callbacks to work

--TODO: Accept a list of the flags and add them and stuff.
{# fun unsafe actor_set_flags as ^
   `(ActorClass self)' => { withActorClass* `self', cFromEnum `ActorFlags' } -> `()' #}
{# fun unsafe actor_unset_flags as ^
   `(ActorClass self)' => { withActorClass* `self', cFromEnum `ActorFlags' } -> `()' #}
{# fun unsafe actor_get_flags as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `ActorFlags' cToEnum #}

{# fun actor_show as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_show_all as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_hide as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_hide_all as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_realize as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_unrealize as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_paint as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_queue_redraw as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun actor_queue_relayout as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}

{# fun unsafe actor_map as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_unmap as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}

{# fun unsafe actor_set_parent as ^
   `(ActorClass child, ActorClass parent)' => { withActorClass* `child', withActorClass* `parent' } -> `()' #}

--TODO: Not sure how to out marshal this
--{# fun unsafe actor_get_parent as ^
--   `(ActorClass child)' => { withActorClass* `child' } -> `Ptr Actor' #}

{# fun unsafe actor_unparent as ^
   `(ActorClass child)' => { withActorClass* `child' } -> `()' #}
{# fun unsafe actor_reparent as ^
   `(ActorClass child, ActorClass newparent)' => { withActorClass* `child', withActorClass* `newparent' } -> `()' #}

{# fun unsafe actor_raise as ^
   `(ActorClass self, ActorClass below)' => { withActorClass* `self', withActorClass* `below' } -> `()' #}
{# fun unsafe actor_lower as ^
   `(ActorClass self, ActorClass below)' => { withActorClass* `self', withActorClass* `below' } -> `()' #}
{# fun unsafe actor_raise_top as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `()' #}
{# fun unsafe actor_lower_bottom as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `()' #}

--{# fun unsafe actor_get_stage as ^
--   `(ActorClass child)' => { withActorClass* `child' } -> `Ptr Actor' #}


{# fun unsafe actor_get_depth as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}
{# fun unsafe actor_set_depth as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}
actorDepth :: (ActorClass self) => Attr self Float
actorDepth = newAttr actorGetDepth actorSetDepth

{# fun unsafe actor_set_scale as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', `Double'} -> `()' #}
{# fun unsafe actor_set_scale_full as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', `Double', `Float', `Float' } -> `()' #}
{# fun unsafe actor_set_scale_with_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', `Double', cFromEnum `Gravity'} -> `()' #}
{# fun unsafe actor_get_scale as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv*} -> `()' #}

{# fun unsafe actor_get_scale_center as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv*} -> `()' #}

{# fun unsafe actor_get_scale_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}
{# fun unsafe actor_is_scaled as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Bool' #}



{# fun unsafe actor_set_position as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}
{# fun unsafe actor_get_position as ^
   `(ActorClass self)' => { withActorClass* `self', alloca- `Float' peekFloatConv*, alloca- `Float' peekFloatConv*} -> `()' #}

--should I tuple these?
--actorPosition :: (ActorClass self) => Attr Self (Float, Float)
--actorPosition = newAttr  actorGetPosition actorSetPosition

--FIXME: lol withActorClass

{# fun unsafe actor_set_size as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}
{# fun unsafe actor_get_size as ^
   `(ActorClass self)' => { withActorClass* `self', alloca- `Float' peekFloatConv*, alloca- `Float' peekFloatConv*} -> `()' #}

{# fun unsafe actor_get_width as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}
{# fun unsafe actor_set_width as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}
actorWidth :: (ActorClass self) => Attr self Float
actorWidth = newAttr actorGetWidth actorSetWidth

{# fun unsafe actor_get_height as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}
{# fun unsafe actor_set_height as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}
actorHeight :: (ActorClass self) => Attr self Float
actorHeight = newAttr actorGetHeight actorSetHeight

{# fun unsafe actor_get_x as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}
{# fun unsafe actor_set_x as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}
actorX :: (ActorClass self) => Attr self Float
actorX = newAttr actorGetX actorSetX

{# fun unsafe actor_get_y as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}
{# fun unsafe actor_set_y as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}
actorY :: (ActorClass self) => Attr self Float
actorY = newAttr actorGetY actorSetY

{# fun unsafe actor_is_in_clone_paint as ^
       `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

{# fun unsafe actor_move_by as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}

{# fun unsafe actor_set_rotation as ^
   `(ActorClass self)' => { withActorClass* `self', cFromEnum `RotateAxis', `Double', `Float', `Float', `Float' } -> `()' #}

{# fun unsafe actor_set_z_rotation_from_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', cFromEnum `Gravity' } -> `()' #}
{# fun unsafe actor_get_rotation as ^
   `(ActorClass self)' => { withActorClass* `self',
                            cFromEnum `RotateAxis',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `Double' #}

{# fun unsafe actor_get_z_rotation_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}

{# fun unsafe actor_is_rotated as ^ `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

--FIXME: All the types for everything
{# fun unsafe actor_set_opacity as ^
   `(ActorClass self)' => { withActorClass* `self', `Word8' } -> `()' #}
{# fun unsafe actor_get_opacity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Word8' #}
actorOpacity :: (ActorClass self) => Attr self Word8
actorOpacity = newAttr actorGetOpacity actorSetOpacity

{# fun unsafe actor_set_name as ^
   `(ActorClass self)' => { withActorClass* `self', `String' } -> `()' #}
{# fun unsafe actor_get_name as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `String' #}
actorName :: (ActorClass self) => Attr self String
actorName = newAttr actorGetName actorSetName

{# fun unsafe actor_get_gid as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Word32' #}
actorGid :: (ActorClass self) => ReadAttr self Word32
actorGid = readAttr actorGetGid

{# fun unsafe actor_set_clip as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float', `Float', `Float' } -> `()' #}
{# fun unsafe actor_get_clip as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `()' #}

{# fun unsafe actor_has_clip as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}
{# fun unsafe actor_remove_clip as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `()' #}


--signals


--FIXME: Event is not an actor, but event doesn't exist right now.
--FIXME: Make the Ptr part go away
onButtonPressEvent:: ActorClass a => a -> (Ptr Event -> IO ()) -> IO (ConnectId a)
onButtonPressEvent = connect_PTR__NONE "button-press-event" False
--onButtonPressEvent = connect_NONE__NONE "button-press-event" False


onDestroy, afterDestroy :: ActorClass a => a -> IO () -> IO (ConnectId a)
onDestroy = connect_NONE__NONE "destroy" False
afterDestroy = connect_NONE__NONE "destroy" True

destroy :: ActorClass self => Signal self (IO ())
destroy = Signal (connect_NONE__NONE "destroy")

onHide, afterHide :: ActorClass a => a -> IO () -> IO (ConnectId a)
onHide = connect_NONE__NONE "hide" False
afterHide = connect_NONE__NONE "hide" True

hide :: ActorClass self => Signal self (IO ())
hide = Signal (connect_NONE__NONE "hide")


onKeyFocusIn, afterKeyFocusIn :: ActorClass a => a -> IO () -> IO (ConnectId a)
onKeyFocusIn = connect_NONE__NONE "key_focus_in" False
afterKeyFocusIn = connect_NONE__NONE "key_focus_in" True

keyFocusIn :: ActorClass self => Signal self (IO ())
keyFocusIn = Signal (connect_NONE__NONE "key_focus_in")


onKeyFocusOut, afterKeyFocusOut :: ActorClass a => a -> IO () -> IO (ConnectId a)
onKeyFocusOut = connect_NONE__NONE "key_focus_out" False
afterKeyFocusOut = connect_NONE__NONE "key_focus_out" True

keyFocusOut :: ActorClass self => Signal self (IO ())
keyFocusOut = Signal (connect_NONE__NONE "key_focus_out")


onPaint, afterPaint :: ActorClass a => a -> IO () -> IO (ConnectId a)
onPaint = connect_NONE__NONE "paint" False
afterPaint = connect_NONE__NONE "paint" True

paint :: ActorClass self => Signal self (IO ())
paint = Signal (connect_NONE__NONE "paint")


onRealize, afterRealize :: ActorClass a => a -> IO () -> IO (ConnectId a)
onRealize = connect_NONE__NONE "realize" False
afterRealize = connect_NONE__NONE "realize" True

realize :: ActorClass self => Signal self (IO ())
realize = Signal (connect_NONE__NONE "realize")

onShow, afterShow :: ActorClass a => a -> IO () -> IO (ConnectId a)
onShow = connect_NONE__NONE "show" False
afterShow = connect_NONE__NONE "show" True

show :: ActorClass self => Signal self (IO ())
show = Signal (connect_NONE__NONE "show")

