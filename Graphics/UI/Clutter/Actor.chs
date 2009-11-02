-- -*-haskell-*-
--  Clutter Actor
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

module Graphics.UI.Clutter.Actor (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GInitiallyUnowned'
-- |         +----'Actor'
-- @
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

                                  actorShouldPickPaint,
                                  actorMap,
                                  actorUnmap,
                                  actorAllocate,
                                  actorAllocatePreferredSize,
                                  actorAllocateAvailableSize,
                                  actorGetAllocationBox,
                                  actorGetAllocationGeometry,
                                  actorGetAllocationVertices,

                                  actorGetPreferredSize,
                                  actorGetPreferredWidth,
                                  actorGetPreferredHeight,
                                  actorSetFixedPositionSet,
                                  actorGetFixedPositionSet,
                                  actorFixedPositionSet,
                                  actorSetGeometry,
                                  actorGetGeometry,
                                  actorGeometry,

                                  actorSetSize,
                                  actorGetSize,
                                  actorSize,

                                  actorSetPosition,
                                  actorGetPosition,
                                  actorPosition,

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
                                  actorGetParent,
                                --actorParent,
                                  actorReparent,
                                  actorUnparent,
                                  actorRaise,
                                  actorLower,
                                  actorRaiseTop,
                                  actorLowerBottom,
                                  actorGetStage,
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
                                  actorApplyTransformToPoint,
                                  actorTransformStagePoint,
                                  actorApplyRelativeTransformToPoint,
                                  actorGetTransformedPosition,
                                  actorGetTransformedSize,
                                  actorTransformedSize,
                                  actorGetPaintOpacity,
                                  actorPaintOpacity,
                                  actorGetPaintVisibility,
                                  actorPaintVisibility,
                                  actorGetAbsAllocationVertices,
                                --actorGetTransformationMatrix,
                                  actorSetAnchorPoint,
                                  actorGetAnchorPoint,
                                  actorAnchorPoint,
                                  actorSetAnchorPointFromGravity,
                                  actorGetAnchorPointGravity,
                                  actorAnchorPointGravity,
                                  actorMoveAnchorPoint,
                                  actorMoveAnchorPointFromGravity,
                                  actorSetReactive,
                                  actorGetReactive,
                                  actorReactive,
                                  actorSetShader,
                                  actorGetShader,
                                  actorShader,
                                --actorSetShaderParam,
                                --actorSetShaderParamFloat,
                                --actorSetShaderParamInt,
                                  actorGrabKeyFocus,
                                --actorGetPangoContext,
                                --actorCreatePangoContext,
                                --actorCreatePangoLayout,
                                  actorIsInClonePaint,

                                --actorBoxNew,
                                --actorBoxCopy,
                                --actorBoxFree,
                                --actorBoxEqual,
                                  actorBoxGetX,
                                  actorBoxGetY,
                                  actorBoxGetWidth,
                                  actorBoxGetHeight,
                                  actorBoxGetOrigin,
                                  actorBoxGetSize,
                                  actorBoxGetArea,
                                  actorBoxContains,
                                  actorBoxFromVertices,
                                --vertexNew,
                                --vertexCopy,
                                --vertexFree,
                                --vertexEqual

                                -- * Signals

                                --onAllocationChanged,
                                --afterAllocationChanged,
                                --allocationChanged,

                                  onDestroy,
                                  afterDestroy,
                                  destroy,

                                  onHide,
                                  afterHide,
                                  hide,

                                  onKeyFocusIn,
                                  afterKeyFocusIn,
                                  keyFocusIn,

                                  onKeyFocusOut,
                                  afterKeyFocusOut,
                                  keyFocusOut,

                                  onPaint,
                                  afterPaint,
                                  paint,

                                  onParentSet,
                                  afterParentSet,
                                  parentSet,

                                  onPick,
                                  afterPick,
                                  pick,

                                  onQueueRedraw,
                                  afterQueueRedraw,
                                  queueRedraw,

                                  onRealize,
                                  afterRealize,
                                  realize,

                                  onShow,
                                  afterShow,
                                  show,

                                  onUnrealize,
                                  afterUnrealize,
                                  unrealize
                                 ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}
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
   `(ActorClass self)' => { withActorClass* `self', cFromFlags `[ActorFlags]' } -> `()' #}

-- | Unset /flags/ on /self/
--   This function will emit notifications for the changed properties.
--
{# fun unsafe actor_unset_flags as ^
   `(ActorClass self)' => { withActorClass* `self', cFromFlags `[ActorFlags]' } -> `()' #}
{# fun unsafe actor_get_flags as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `[ActorFlags]' cToFlags #}

{# fun actor_show as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_show_all as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_hide as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_hide_all as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_realize as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_unrealize as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_paint as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_queue_redraw as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun actor_queue_relayout as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}

{# fun unsafe actor_should_pick_paint as ^ `(ActorClass o)' => { withActorClass* `o'} -> `Bool' #}
{# fun unsafe actor_map as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}
{# fun unsafe actor_unmap as ^ `(ActorClass o)' => { withActorClass* `o'} -> `()' #}


{# fun unsafe actor_allocate as ^
 `(ActorClass self)' => { withActorClass* `self',
                          withActorBox* `ActorBox',
                          cFromFlags `[AllocationFlags]'
                        } -> `()' #}

{# fun unsafe actor_allocate_preferred_size as ^
       `(ActorClass self)' => { withActorClass* `self',
                                cFromFlags `[AllocationFlags]'
                              } -> `()' #}

{# fun unsafe actor_allocate_available_size as ^
       `(ActorClass self)' => { withActorClass* `self',
                                `Float',
                                `Float',
                                `Float',
                                `Float',
                                cFromFlags `[AllocationFlags]'
                              } -> `()' #}

{# fun unsafe actor_get_allocation_box as ^
       `(ActorClass self)' => { withActorClass* `self',
                                alloca- `ActorBox' peek* } -> `()' #}

{# fun unsafe actor_get_allocation_geometry as ^
       `(ActorClass self)' => { withActorClass* `self',
                                alloca- `Geometry' peek*
                              } -> `()' #}

actorGetAllocationVertices  :: (ActorClass self, ActorClass ancestor) => self -> ancestor -> IO [Vertex]
actorGetAllocationVertices self ancestor = let func = {# call unsafe actor_get_allocation_vertices #}
                                           in
                                             withActorClass self $ \selfPtr ->
                                               withActorClass ancestor $ \ancPtr ->
                                               allocaArray 4 $ \vsPtr -> do
                                                 func selfPtr ancPtr vsPtr
                                                 peekArray 4 vsPtr

{# fun unsafe actor_get_preferred_size as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}
--TODO: Do I want this? Others take another param, would be inconsisent to have just this one
--actorPreferredSize :: (ActorClass self) => ReadAttr self (Float, Float, Float, Float)
--actorPreferredSize = readAttr actorGetPreferredSize

{# fun unsafe actor_get_preferred_width as ^
   `(ActorClass self)' => { withActorClass* `self',
                            `Float',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}

{# fun unsafe actor_get_preferred_height as ^
   `(ActorClass self)' => { withActorClass* `self',
                            `Float',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}

{# fun unsafe actor_set_fixed_position_set as ^
       `(ActorClass self)' => { withActorClass* `self', `Bool'} -> `()' #}
{# fun unsafe actor_get_fixed_position_set as ^
 `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}
actorFixedPositionSet :: (ActorClass self) => Attr self Bool
actorFixedPositionSet = newAttr actorGetFixedPositionSet actorSetFixedPositionSet

{# fun unsafe actor_get_geometry as ^
       `(ActorClass self)' => { withActorClass* `self', alloca- `Geometry' peek* } -> `()' #}
{# fun unsafe actor_set_geometry as ^
       `(ActorClass self)' => { withActorClass* `self', withGeometry* `Geometry' } -> `()' #}
actorGeometry :: (ActorClass self) => Attr self Geometry
actorGeometry = newAttr actorGetGeometry actorSetGeometry

{# fun unsafe actor_set_size as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}
{# fun unsafe actor_get_size as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `()' #}

actorSize :: (ActorClass self) => Attr self (Float, Float)
actorSize = newAttr actorGetSize (tup2ToF actorSetSize)

{# fun unsafe actor_set_position as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}
{# fun unsafe actor_get_position as ^
   `(ActorClass self)' => { withActorClass* `self', alloca- `Float' peekFloatConv*, alloca- `Float' peekFloatConv*} -> `()' #}

actorPosition :: (ActorClass self) => Attr self (Float, Float)
actorPosition = newAttr actorGetPosition (tup2ToF actorSetPosition)

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


{# fun unsafe actor_set_parent as ^
   `(ActorClass child, ActorClass parent)' => { withActorClass* `child', withActorClass* `parent' } -> `()' #}

{# fun unsafe actor_get_parent as ^
   `(ActorClass child)' => { withActorClass* `child' } -> `Actor' newActor* #}
--actorParent :: (ActorClass self, ActorClass parent) => Attr self parent
--actorParent = newAttr actorGetPartent actorSetParent


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

{# fun unsafe actor_get_stage as ^
   `(ActorClass child)' => { withActorClass* `child' } -> `Stage' newStage* #}


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
                            alloca- `Double' peekFloatConv* } -> `()' #}

{# fun unsafe actor_get_scale_center as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv* } -> `()' #}

{# fun unsafe actor_get_scale_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}
{# fun unsafe actor_is_scaled as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Bool' #}

{# fun unsafe actor_apply_transform_to_point as ^
   `(ActorClass self)' => { withActorClass* `self',
                            withVertex* `Vertex',
                            alloca- `Vertex' peek*
                          } -> `()' #}


--CHECKME: unsafe?
{# fun unsafe actor_transform_stage_point as ^
   `(ActorClass a)' => { withActorClass* `a',
                         `Float',
                         `Float',
                         alloca- `Float' peekFloatConv*,
                         alloca- `Float' peekFloatConv* } ->
                         `Bool' #}


{# fun unsafe actor_apply_relative_transform_to_point as ^
   `(ActorClass self, ActorClass ancestor)' => { withActorClass* `self',
                                                 withActorClass* `ancestor',
                                                 withVertex* `Vertex',
                                                 alloca- `Vertex' peek*
                                               } -> `()' #}

{# fun unsafe actor_get_transformed_position as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}

{# fun unsafe actor_get_transformed_size as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv* } -> `()' #}
actorTransformedSize :: (ActorClass self) => ReadAttr self (Double, Double)
actorTransformedSize = readAttr actorGetTransformedSize

{# fun unsafe actor_get_paint_opacity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Word8' #}
actorPaintOpacity :: (ActorClass self) => ReadAttr self Word8
actorPaintOpacity = readAttr actorGetPaintOpacity

{# fun unsafe actor_get_paint_visibility as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}
actorPaintVisibility :: (ActorClass self) => ReadAttr self Bool
actorPaintVisibility = readAttr actorGetPaintVisibility


actorGetAbsAllocationVertices  :: (ActorClass self) => self -> IO [Vertex]
actorGetAbsAllocationVertices self = let func = {# call unsafe actor_get_abs_allocation_vertices #}
                                     in
                                       withActorClass self $ \selfPtr ->
                                           allocaArray 4 $ \vsPtr -> do
                                             func selfPtr vsPtr
                                             peekArray 4 vsPtr

{# fun unsafe actor_set_reactive as ^
   `(ActorClass self)' => { withActorClass* `self', `Bool' } -> `()' #}
{# fun unsafe actor_get_reactive as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}
actorReactive :: (ActorClass self) => Attr self Bool
actorReactive = newAttr actorGetReactive actorSetReactive

{# fun unsafe actor_set_shader as ^
   `(ActorClass self)' => { withActorClass* `self', withShader* `Shader' } -> `()' #}
{# fun unsafe actor_get_shader as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Shader' newShader* #}
actorShader :: (ActorClass self) => Attr self Shader
actorShader = newAttr actorGetShader actorSetShader

--{# fun unsafe actor_set_shader_param as ^
--{# fun unsafe actor_set_shader_param_float as ^
--{# fun unsafe actor_set_shader_param_int as ^


--CHECKME: unsafe?
{# fun unsafe actor_grab_key_focus as ^
   `(ActorClass a)' => { withActorClass* `a' } -> `()' #}

{-
--TODO: I think I figured out the pango stuff in Text
{# fun unsafe actor_get_pango_context as ^
   `(ActorClass a)' => { withActorClass* `a' } -> `PangoContext' newPangoContext* #}
{# fun unsafe actor_create_pango_context as ^
   `(ActorClass a)' => { withActorClass* `a' } -> `PangoContext' newPangoContext* #}
{# fun unsafe actor_create_pango_layout as ^
   `(ActorClass a)' => { withActorClass* `a', `String' } -> `PangoLayout' newPangoLayout* #}
-}

{# fun unsafe actor_is_in_clone_paint as ^
       `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}


{# fun unsafe actor_set_anchor_point as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}
{# fun unsafe actor_get_anchor_point as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `()' #}

actorAnchorPoint :: (ActorClass self) => Attr self (Float, Float)
actorAnchorPoint = newAttr actorGetAnchorPoint (tup2ToF actorSetAnchorPoint)

{# fun unsafe actor_set_anchor_point_from_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', cFromEnum `Gravity' } -> `()' #}
{# fun unsafe actor_get_anchor_point_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}

actorAnchorPointGravity :: (ActorClass self) => Attr self Gravity
actorAnchorPointGravity = newAttr actorGetAnchorPointGravity actorSetAnchorPointFromGravity

{# fun unsafe actor_move_anchor_point as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}
{# fun unsafe actor_move_anchor_point_from_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', cFromEnum `Gravity' } -> `()' #}



--TODO: Check they are pure. Also wouldn't be hard to actually haskellize them
{# fun pure unsafe actor_box_get_x as ^ { withActorBox* `ActorBox' } -> `Float' #}
{# fun pure unsafe actor_box_get_y as ^ { withActorBox* `ActorBox' } -> `Float' #}
{# fun pure unsafe actor_box_get_width as ^ { withActorBox* `ActorBox' } -> `Float' #}
{# fun pure unsafe actor_box_get_height as ^ { withActorBox* `ActorBox' } -> `Float' #}
{# fun pure unsafe actor_box_get_origin as ^ { withActorBox* `ActorBox',
                                               alloca- `Float' peekFloatConv*,
                                               alloca- `Float' peekFloatConv* } -> `()' #}
{# fun pure unsafe actor_box_get_size as ^ { withActorBox* `ActorBox',
                                             alloca- `Float' peekFloatConv*,
                                             alloca- `Float' peekFloatConv* } -> `()' #}
{# fun pure unsafe actor_box_get_area as ^ { withActorBox* `ActorBox' } -> `Float' #}

{# fun pure unsafe actor_box_contains as ^ { withActorBox* `ActorBox',
                                             `Float',
                                             `Float' } -> `()' #}

{# fun pure unsafe actor_box_from_vertices as ^ { withActorBox* `ActorBox',
                                                  withArray* `[Vertex]'
                                                } -> `()' #}

{-
--CHECKME/FIXME: allocation flags enum magic maybe break things, need flags thing to happen
onAllocationChanged, afterAllocationChanged :: ActorClass a => a
                                            -> (ActorBox -> AllocationFlags -> IO ())
                                            -> IO (ConnectId a)
onAllocationChanged = connect_BOXED_ENUM__NONE "allocation_changed" False
afterAllocationChanged = connect_BOXED_ENUM__NONE "allocation_changed" True

allocationChanged :: ActorClass self => Signal self (ActorBox -> AllocationFlags -> IO ())
allocationChanged = Signal (connect_BOXED_ENUM__NONE "destroy")
-}

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


onParentSet, afterParentSet :: ActorClass a => a -> (Actor -> IO ()) -> IO (ConnectId a)
onParentSet = connect_OBJECT__NONE "parent_set" False
afterParentSet = connect_OBJECT__NONE "parent_set" True

parentSet :: ActorClass self => Signal self (Actor -> IO ())
parentSet = Signal (connect_OBJECT__NONE "parent")


onPick, afterPick :: ActorClass a => a -> (Color -> IO ()) -> IO (ConnectId a)
onPick = connect_BOXED__NONE "pick" peek False
afterPick = connect_BOXED__NONE "pick" peek True

pick :: ActorClass self => Signal self (Color -> IO ())
pick = Signal (connect_BOXED__NONE "pick" peek)


onQueueRedraw, afterQueueRedraw :: ActorClass a => a -> (Actor -> IO ()) -> IO (ConnectId a)
onQueueRedraw = connect_OBJECT__NONE "queue_redraw" False
afterQueueRedraw = connect_OBJECT__NONE "queue_redraw" True

queueRedraw :: ActorClass self => Signal self (Color -> IO ())
queueRedraw = Signal (connect_BOXED__NONE "queue_redraw" peek)


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

onUnrealize, afterUnrealize :: ActorClass a => a -> IO () -> IO (ConnectId a)
onUnrealize = connect_NONE__NONE "unrealize" False
afterUnrealize = connect_NONE__NONE "unrealize" True

unrealize :: ActorClass self => Signal self (IO ())
unrealize = Signal (connect_NONE__NONE "unrealize")


