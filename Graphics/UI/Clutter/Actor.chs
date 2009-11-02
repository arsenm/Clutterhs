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

-- * Methods
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
  actorDestroy,

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


  actorSetSize,
  actorGetSize,

  actorSetPosition,
  actorGetPosition,

  actorSetWidth,
  actorGetWidth,
  actorSetHeight,
  actorGetHeight,
  actorSetX,
  actorGetX,

  actorSetY,
  actorGetY,


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


  actorGetGid,

  actorSetClip,
  actorRemoveClip,
  actorHasClip,
  actorGetClip,

  actorSetParent,
  actorGetParent,

  actorReparent,
  actorUnparent,
  actorRaise,
  actorLower,
  actorRaiseTop,
  actorLowerBottom,
  actorGetStage,
  actorSetDepth,
  actorGetDepth,

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

  actorGetPaintOpacity,
  actorGetPaintVisibility,
  actorPaintVisibility,
  actorGetAbsAllocationVertices,
--actorGetTransformationMatrix,
  actorSetAnchorPoint,
  actorGetAnchorPoint,
  actorSetAnchorPointFromGravity,
  actorGetAnchorPointGravity,
  actorMoveAnchorPoint,
  actorMoveAnchorPointFromGravity,
  actorSetReactive,
  actorGetReactive,

  actorSetShader,
  actorGetShader,

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

-- * Attributes
  actorGeometry,
  actorSize,
  actorPosition,
  actorWidth,
  actorHeight,
  actorX,
  actorY,
  actorName,
  actorGid,
--actorParent,
  actorDepth,
  actorTransformedSize,
  actorPaintOpacity,
  actorAnchorPoint,
  actorAnchorPointGravity,
  actorReactive,
  actorShader,

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

-- | Sets flags on self
--
-- This function will emit notifications for the changed properties
--
-- [@self@] an actor
--
-- [@flags@] a list of flags to set
--
-- * Since 1.0
--
{# fun unsafe actor_set_flags as ^
   `(ActorClass self)' => { withActorClass* `self', cFromFlags `[ActorFlags]' } -> `()' #}

-- | Unset /flags/ on /self/
--
--   This function will emit notifications for the changed properties.
--
-- * Since 1.0
--
{# fun unsafe actor_unset_flags as ^
   `(ActorClass self)' => { withActorClass* `self', cFromFlags `[ActorFlags]' } -> `()' #}

-- | Retrieves the flags set on an actor
--
-- [@self@] an actor
--
-- [@Returns@] a list of set flags
--
-- * Since 1.0
--
{# fun unsafe actor_get_flags as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `[ActorFlags]' cToFlags #}

-- | Flags an actor to be displayed. An actor that isn't shown will not be rendered on the stage.
--
-- Actors are visible by default.
--
-- If this function is called on an actor without a parent, the
-- "show-on-set-parent" will be set to @True@ as a side effect.
--
{# fun actor_show as ^ `(ActorClass self)' => { withActorClass* `self'} -> `()' #}

-- | Calls actor'Show' on all children of an actor (if any).
--
-- * Since 0.2
--
{# fun actor_show_all as ^ `(ActorClass self)' => { withActorClass* `self'} -> `()' #}

-- | Flags an actor to be hidden. A hidden actor will not be rendered on the stage.
--
-- Actors are visible by default.
--
-- If this function is called on an actor without a parent, the
-- "show-on-set-parent" property will be set to @False@ as a
-- side-effect.
--
{# fun actor_hide as ^ `(ActorClass self)' => { withActorClass* `self'} -> `()' #}

-- | Calls 'actorHide' on all child actors (if any).
--
-- * Since 0.2
--
{# fun actor_hide_all as ^ `(ActorClass self)' => { withActorClass* `self'} -> `()' #}

-- | Creates any underlying graphics resources needed by the actor to be displayed.
--
-- Realization means the actor is now tied to a specific rendering
-- context (that is, a specific toplevel stage).
--
-- This function does nothing if the actor is already realized.
--
-- Because a realized actor must have realized parent actors, calling
-- 'actorRealize' will also realize all parents of the actor.
--
-- This function does not realize child actors, except in the special
-- case that realizing the stage, when the stage is visible, will
-- suddenly map (and thus realize) the children of the stage.
--
{# fun actor_realize as ^ `(ActorClass self)' => { withActorClass* `self' } -> `()' #}

-- | Frees up any underlying graphics resources needed by the actor to be displayed.
--
-- Unrealization means the actor is now independent of any specific
-- rendering context (is not attached to a specific toplevel stage).
--
-- Because mapped actors must be realized, actors may not be unrealized
-- if they are mapped. This function hides the actor to be sure it
-- isn't mapped, an application-visible side effect that you may not be
-- expecting.
--
-- This function should not really be in the public API, because there
-- isn't a good reason to call it. ClutterActor will already unrealize
-- things for you when it's important to do so.
--
{# fun actor_unrealize as ^ `(ActorClass self)' => { withActorClass* `self' } -> `()' #}


-- | Renders the actor to display.
--
-- This function should not be called directly by applications. Call
-- 'actorQueueRedraw' to queue paints, instead.
--
-- This function will emit the 'paint' signal.
--
{# fun actor_paint as ^ `(ActorClass self)' => { withActorClass* `self' } -> `()' #}

-- | Queues up a redraw of an actor and any children. The redraw
--   occurs once the main loop becomes idle (after the current batch
--   of events has been processed, roughly).
--
-- Applications rarely need to call this, as redraws are handled
-- automatically by modification functions.
--
-- This function will not do anything if self is not visible, or if
-- the actor is inside an invisible part of the scenegraph.
--
-- Also be aware that painting is a NOP for actors with an opacity of 0
--
{# fun actor_queue_redraw as ^ `(ActorClass self)' => { withActorClass* `self' } -> `()' #}

-- | Indicates that the actor's size request or other layout-affecting
--   properties may have changed. This function is used inside
--   'Actor' subclass implementations, not by applications
--   directly.
--
--
-- Queueing a new layout automatically queues a redraw as well.
--
-- * Since 0.8
--
{# fun actor_queue_relayout as ^ `(ActorClass self)' => { withActorClass* `self'} -> `()' #}

-- | Destroys an actor. When an actor is destroyed, it will break any
--   references it holds to other objects. If the actor is inside a
--   container, the actor will be removed.
--
-- When you destroy a container, its children will be destroyed as well.
--
-- Note: you cannot destroy the 'Stage' returned by 'stageGetDefault'.
--
{# fun actor_destroy as ^  `(ActorClass self)' => { withActorClass* `self'} -> `()' #}

{-
-- | This function is used to emit an event on the main stage. You
--   should rarely need to use this function, except for synthetising
--   events.
--
-- [@actor@] an actor
--
-- [@event@] an Event
--
-- [@capture@]  @True@ if event in in capture phase, @False@ otherwise.
--
-- [@Returns@] the return value from the signal emission: @True@ if
-- the actor handled the event, or @False@ if the event was not handled
--
-- *  Since 0.6
--
{# fun actor_event as ^  `(ActorClass self)' => { withActorClass* `self',
                                                  `Event',
                                                  `Bool' } -> `()' #}
-}

-- | Should be called inside the implementation of the "pick" virtual
--   function in order to check whether the actor should paint itself
--   in pick mode or not.
--
-- This function should never be called directly by applications.
--
-- [@self@] An Actor
--
-- [@Returns@] @True@ if the actor should paint its silhouette, @False@ otherwise
--
{# fun unsafe actor_should_pick_paint as ^ `(ActorClass self)' => { withActorClass* `self'} -> `Bool' #}

-- | Sets the 'ActorMapped' flag on the actor and possibly maps
--   and realizes its children if they are visible. Does nothing if
--   the actor is not visible.
--
-- Calling this is allowed in only one case: you are implementing the
-- "map" virtual function in an actor and you need to map the children
-- of that actor. It is not necessary to call this if you implement
-- ClutterContainer because the default implementation will
-- automatically map children of containers.
--
-- When overriding map, it is mandatory to chain up to the parent implementation.
--
-- * Since 1.0
--
{# fun unsafe actor_map as ^ `(ActorClass self)' => { withActorClass* `self' } -> `()' #}


-- | Unsets the 'ActorMapped' flag on the actor and possibly
--  unmaps its children if they were mapped.
--
--   Calling this is allowed in only one case: you are implementing
--   the "unmap" virtual function in an actor and you need to unmap
--   the children of that actor. It is not necessary to call this if
--   you implement ClutterContainer because the default implementation
--   will automatically unmap children of containers.
--
-- When overriding unmap, it is mandatory to chain up to the parent implementation.
--
-- * Since 1.0
--
{# fun unsafe actor_unmap as ^ `(ActorClass self)' => { withActorClass* `self' } -> `()' #}


-- | Called by the parent of an actor to assign the actor its
--   size. Should never be called by applications (except when
--   implementing a container or layout manager).
--
-- Actors can know from their allocation box whether they have moved
-- with respect to their parent actor. The flags parameter describes
-- additional information about the allocation, for instance whether
-- the parent has moved with respect to the stage, for example
-- because a grandparent's origin has moved.
--
-- [@self@] An Actor
--
-- [@box@]: new allocation of the actor, in parent-relative coordinates
--
-- [@flags@] list of flags that control the allocation
--
-- * Since 0.8
--
{# fun unsafe actor_allocate as ^
 `(ActorClass self)' => { withActorClass* `self',
                          withActorBox* `ActorBox',
                          cFromFlags `[AllocationFlags]'
                        } -> `()' #}


-- | Allocates the natural size of self.
--
-- This function is a utility call for ClutterActor implementations
-- that allocates the actor's preferred natural size. It can be used
-- by fixed layout managers (like 'Group' or so called 'composite
-- actors') inside the ClutterActor::allocate implementation to give
-- each child exactly how much space it requires.
--
-- This function is not meant to be used by applications. It is also
-- not meant to be used outside the implementation of the
-- ClutterActor::allocate virtual function.
--
-- [@self@] an Actor
--
-- [@flags@] flags controlling the allocation
--
-- * Since 0.8
--
{# fun unsafe actor_allocate_preferred_size as ^
       `(ActorClass self)' => { withActorClass* `self',
                                cFromFlags `[AllocationFlags]'
                              } -> `()' #}

--TODO: Redo the example function
-- | Allocates self taking into account the Actor's preferred
--   size, but limiting it to the maximum available width and height
--   provided.
--
-- This function will do the right thing when dealing with the actor's request mode.
--
-- This function can be used by fluid layout managers to allocate an
-- actor's preferred size without making it bigger than the area
-- available for the container.
--
-- [@self@] an Actor
--
-- [@x@] the actor's X coordinate
--
-- [@y@] the actor's Y coordinate
--
-- [@available_width@] the maximum available width, or -1 to use the actor's natural width
--
-- [@available_height@] the maximum available height, or -1 to use the actor's natural height
--
-- [@flags@] list of flags controlling the allocation
--
-- * Since 1.0
--
{# fun unsafe actor_allocate_available_size as ^
       `(ActorClass self)' => { withActorClass* `self',
                                `Float',
                                `Float',
                                `Float',
                                `Float',
                                cFromFlags `[AllocationFlags]'
                              } -> `()' #}


-- | Gets the layout box an actor has been assigned. The allocation
--   can only be assumed valid inside a paint() method; anywhere else,
--   it may be out-of-date.
--
-- An allocation does not incorporate the actor's scale or anchor
-- point; those transformations do not affect layout, only rendering.
--
-- Note
--
-- Do not call any of the actorGetAllocation* family of
-- functions inside the implementation of the 'getPreferredWidth' or
-- 'getPreferredHeight' virtual functions.
--
-- [@self@] An Actor
--
-- [@box@] the function fills this in with the actor's allocation. out.
--
-- * Since 0.8
--
{# fun unsafe actor_get_allocation_box as ^
       `(ActorClass self)' => { withActorClass* `self',
                                alloca- `ActorBox' peek* } -> `()' #}



-- | Gets the layout box an actor has been assigned. The allocation
--   can only be assumed valid inside a paint() method; anywhere else,
--   it may be out-of-date.
--
-- An allocation does not incorporate the actor's scale or anchor
-- point; those transformations do not affect layout, only rendering.
--
-- The returned rectangle is in pixels.
--
-- [@self@] An Actor
--
-- [@Returns@] allocation geometry in pixels
--
-- * Since 0.8
--
{# fun unsafe actor_get_allocation_geometry as ^
       `(ActorClass self)' => { withActorClass* `self',
                                alloca- `Geometry' peek*
                              } -> `()' #}


--TODO Fields of the VertexBox type instead of verts[n]
-- | Calculates the transformed coordinates of the four corners of the
--   actor in the plane of ancestor. The returned vertices relate to
--   the 'ActorBox' coordinates as follows:
--
-- * verts[0] contains (x1, y1)
--
-- * verts[1] contains (x2, y1)
--
-- * verts[2] contains (x1, y2)
--
-- * verts[3] contains (x2, y2)
--
-- If ancestor is NULL the ancestor will be the 'Stage'. In this
-- case, the coordinates returned will be the coordinates on the stage
-- before the projection is applied. This is different from the
-- behaviour of 'actorGetAbsAllocationVertices'.
--
-- * Since 0.6
--
actorGetAllocationVertices  :: (ActorClass self, ActorClass ancestor) =>
                               self
                            -> ancestor
                            -> IO [Vertex]
actorGetAllocationVertices self ancestor = let func = {# call unsafe actor_get_allocation_vertices #}
                                           in
                                             withActorClass self $ \selfPtr ->
                                               withActorClass ancestor $ \ancPtr ->
                                               allocaArray 4 $ \vsPtr -> do
                                                 func selfPtr ancPtr vsPtr
                                                 peekArray 4 vsPtr



-- | Computes the preferred minimum and natural size of an actor,
--   taking into account the actor's geometry management (either
--   height-for-width or width-for-height).
--
-- The width and height used to compute the preferred height and
-- preferred width are the actor's natural ones.
--
-- If you need to control the height for the preferred width, or the
-- width for the preferred height, you should use
-- 'actorGetPreferredWidth' and
-- 'actorGetPreferredHeight', and check the actor's
-- preferred geometry management using the "request-mode" property.
--
-- * Since 0.8
--
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




-- | Computes the requested minimum and natural widths for an actor,
--   optionally depending on the specified height, or if they are
--   already computed, returns the cached values.
--
-- An actor may not get its request - depending on the layout manager that's in effect.
--
-- A request should not incorporate the actor's scale or anchor point;
-- those transformations do not affect layout, only rendering.
--
-- [@self@] An Actor
--
-- [@for_height@] available height when computing the preferred width,
-- or a negative value to indicate that no height is defined
--
-- [@min_width_p@]
--
-- [@natural_width_p@]
--
-- * Since 0.8
--
{# fun unsafe actor_get_preferred_width as ^
   `(ActorClass self)' => { withActorClass* `self',
                            `Float',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}


-- | Computes the requested minimum and natural heights for an actor,
--   or if they are already computed, returns the cached values.
--
-- An actor may not get its request - depending on the layout manager
-- that's in effect.
--
-- A request should not incorporate the actor's scale or anchor point;
-- those transformations do not affect layout, only rendering.
--
-- [@self@] An Actor
--
-- [@for_width@] available width to assume in computing desired
-- height, or a negative value to indicate that no width is defined
--
-- [@min_height_p@]
--
-- [@ natural_height_p@]
--
-- * Since 0.8
--
{# fun unsafe actor_get_preferred_height as ^
   `(ActorClass self)' => { withActorClass* `self',
                            `Float',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}

-- | Sets whether an actor has a fixed position set (and will thus be
--   unaffected by any layout manager).
--
-- [@self@] An Actor
--
-- [@is_set@] whether to use fixed position
--
-- * Since 0.8
--
{# fun unsafe actor_set_fixed_position_set as ^
       `(ActorClass self)' => { withActorClass* `self', `Bool'} -> `()' #}

-- | Checks whether an actor has a fixed position set (and will thus
--   be unaffected by any layout manager).
--
-- [@self@] An Actor
--
-- [@Returns@] @True@ if the fixed position is set on the actor
--
-- * Since 0.8
--
{# fun unsafe actor_get_fixed_position_set as ^
 `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

actorFixedPositionSet :: (ActorClass self) => Attr self Bool
actorFixedPositionSet = newAttr actorGetFixedPositionSet actorSetFixedPositionSet

-- | Gets the size and position of an actor relative to its parent
--   actor. This is the same as calling 'actorGetPosition'
--   and 'actorGetSize'. It tries to \"do what you mean\" and
--   get the requested size and position if the actor's allocation is
--   invalid.
--
{# fun unsafe actor_get_geometry as ^
       `(ActorClass self)' => { withActorClass* `self', alloca- `Geometry' peek* } -> `()' #}

-- | Sets the actor's fixed position and forces its minimum and
--   natural size, in pixels. This means the untransformed actor |
--   will have the given geometry. This is the same as calling |
--   'actorSetPosition' and 'actorSetSize'.
--
{# fun unsafe actor_set_geometry as ^
       `(ActorClass self)' => { withActorClass* `self', withGeometry* `Geometry' } -> `()' #}
actorGeometry :: (ActorClass self) => Attr self Geometry
actorGeometry = newAttr actorGetGeometry actorSetGeometry


-- | Sets the actor's size request in pixels. This overrides any
--   \"normal\" size request the actor would have. For example a text
--   actor might normally request the size of the text; this function
--   would force a specific size instead.
--
-- If width and/or height are -1 the actor will use its \"normal\" size
-- request instead of overriding it, i.e. you can \"unset\" the size
-- with -1.
--
-- This function sets or unsets both the minimum and natural size.
--
-- [@self@] An Actor
--
-- [@width@] New width of actor in pixels, or -1
--
-- [@height@] New height of actor in pixels, or -1
--
{# fun unsafe actor_set_size as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}

-- | This function tries to \"do what you mean\" and return the size an
--   actor will have. If the actor has a valid allocation, the
--   allocation will be returned; otherwise, the actors natural size
--   request will be returned.
--
-- If you care whether you get the request vs. the allocation, you
-- should probably call a different function like
-- 'actorGetAllocationBox' or 'actorGetPreferredWidth'.
--
-- * Since 0.2
--
{# fun unsafe actor_get_size as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `()' #}

actorSize :: (ActorClass self) => Attr self (Float, Float)
actorSize = newAttr actorGetSize (tup2ToF actorSetSize)


-- | Sets the actor's fixed position in pixels relative to any parent actor.
--
-- If a layout manager is in use, this position will override the
-- layout manager and force a fixed position.
--
-- [@self@] An Actor
--
-- [@x@] New left position of actor in pixels.
--
-- [@y@] New top position of actor in pixels.
--
{# fun unsafe actor_set_position as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}

-- | This function tries to \"do what you mean\" and tell you where the
--   actor is, prior to any transformations. Retrieves the fixed
--   position of an actor in pixels, if one has been set; otherwise,
--   if the allocation is valid, returns the actor's allocated
--   position; otherwise, returns 0,0.
--
-- The returned position is in pixels.
--
-- * Since 0.6
--
{# fun unsafe actor_get_position as ^
   `(ActorClass self)' => { withActorClass* `self', alloca- `Float' peekFloatConv*, alloca- `Float' peekFloatConv*} -> `()' #}

actorPosition :: (ActorClass self) => Attr self (Float, Float)
actorPosition = newAttr actorGetPosition (tup2ToF actorSetPosition)



-- | Forces a width on an actor, causing the actor's preferred width
--   and height (if any) to be ignored.
--
-- This function sets both the minimum and natural size of the actor.
--
-- [@self@] An Actor
--
-- [@width@] Requested new width for the actor, in pixels
--
-- * Since 0.2
--
{# fun unsafe actor_set_width as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}


-- | Retrieves the width of an Actor.
--
-- If the actor has a valid allocation, this function will return the
-- width of the allocated area given to the actor.
--
-- If the actor does not have a valid allocation, this function will
-- return the actor's natural width, that is the preferred width of
-- the actor.
--
-- If you care whether you get the preferred width or the width that
-- has been assigned to the actor, you should probably call a
-- different function like 'actorGetAllocationBox' to retrieve the
-- allocated size or 'actorGetPreferredWidth' to retrieve the
-- preferred width.
--
-- If an actor has a fixed width, for instance a width that has been
-- assigned using clutter_actor_set_width(), the width returned will
-- be the same value.
--
-- [@self@] An Actor
--
-- [@Returns@] the width of the actor, in pixels
--
{# fun unsafe actor_get_width as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}

actorWidth :: (ActorClass self) => Attr self Float
actorWidth = newAttr actorGetWidth actorSetWidth

-- | Forces a height on an actor, causing the actor's preferred width
--   and height (if any) to be ignored.
--
-- This function sets both the minimum and natural size of the actor.
--
-- [@self@] An Actor
--
-- [@height@] Requested new height for the actor, in pixels
--
-- * Since 0.2
--
{# fun unsafe actor_set_height as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}


-- | Retrieves the height of a ClutterActor.
--
-- If the actor has a valid allocation, this function will return the
-- height of the allocated area given to the actor.
--
-- If the actor does not have a valid allocation, this function will
-- return the actor's natural height, that is the preferred height of
-- the actor.
--
-- If you care whether you get the preferred height or the height that
-- has been assigned to the actor, you should probably call a
-- different function like clutter_actor_get_allocation_box() to
-- retrieve the allocated size or clutter_actor_get_preferred_height()
-- to retrieve the preferred height.
--
-- If an actor has a fixed height, for instance a height that has been
-- assigned using 'actorSetHeight', the height returned will be the
-- same value.
--
-- [@self@] An Actor
--
-- [@Returns@] the height of the actor, in pixels
--
{# fun unsafe actor_get_height as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}

actorHeight :: (ActorClass self) => Attr self Float
actorHeight = newAttr actorGetHeight actorSetHeight

-- | Sets the actor's X coordinate, relative to its parent, in pixels.
--
-- Overrides any layout manager and forces a fixed position for the actor.
--
-- [@self@] an Actor
--
-- [@x@] the actor's position on the X axis
--
-- * Since 0.6
--
{# fun unsafe actor_set_x as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}


-- | Retrieves the X coordinate of an Actor.
--
-- This function tries to \"do what you mean\", by returning the correct
-- value depending on the actor's state.
--
-- If the actor has a valid allocation, this function will return the
-- X coordinate of the origin of the allocation box.
--
-- If the actor has any fixed coordinate set using 'actorSetX',
-- 'actorSetPosition' or 'actorSetGeometry', this function will return
-- that coordinate.
--
-- If both the allocation and a fixed position are missing, this
-- function will return 0.
--
-- [@self@] An Actor
--
-- [@Returns@] the X coordinate, in pixels, ignoring any
-- transformation (i.e. scaling, rotation)
--
{# fun unsafe actor_get_x as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}

actorX :: (ActorClass self) => Attr self Float
actorX = newAttr actorGetX actorSetX


-- | Sets the actor's Y coordinate, relative to its parent, in
--   pixels.
--
-- Overrides any layout manager and forces a fixed position for the actor.
--
-- [@self@] an Actor
--
-- [@y@] the actor's position on the Y axis
--
-- * Since 0.6
--
{# fun unsafe actor_set_y as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}

-- | Retrieves the Y coordinate of an Actor.
--
-- This function tries to \"do what you mean\", by returning the correct
-- value depending on the actor's state.
--
-- If the actor has a valid allocation, this function will return the
-- Y coordinate of the origin of the allocation box.
--
-- If the actor has any fixed coordinate set using 'actorSetY',
-- 'actorSetPosition' or 'actorSetGeometry', this function will return
-- that coordinate.
--
-- If both the allocation and a fixed position are missing, this function will return 0.
--
-- [@self@] An Actor
--
-- [@Returns@] the Y coordinate, in pixels, ignoring any
-- transformation (i.e. scaling, rotation)
--
{# fun unsafe actor_get_y as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}

actorY :: (ActorClass self) => Attr self Float
actorY = newAttr actorGetY actorSetY


-- | Moves an actor by the specified distance relative to its current
--   position in pixels.
--
-- This function modifies the fixed position of an actor and thus
-- removes it from any layout management. Another way to move an actor
-- is with an anchor point, see clutter_actor_set_anchor_point().
--
-- [@self@] An Actor
--
-- [@dx@] Distance to move Actor on X axis.
--
-- [@dy@] Distance to move Actor on Y axis.
--
-- * Since 0.2
--
{# fun unsafe actor_move_by as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}

-- | The rotation center coordinates used depend on the value of axis:
--
-- * 'XAxis' requires y and z
--
-- * 'YAxis' requires x and z
--
-- * 'ZAxis' requires x and y
--
-- The rotation coordinates are relative to the anchor point of the
-- actor, set using 'actorSetAnchorPoint'. If no anchor
-- point is set, the upper left corner is assumed as the origin.
--
-- [@self@] an Actor
--
-- [@axis@] the axis of rotation
--
-- [@angle@] the angle of rotation
--
-- [@x@] X coordinate of the rotation center
--
-- [@y@] Y coordinate of the rotation center
--
-- [@z@] Z coordinate of the rotation center
--
-- * Since 0.8
--
{# fun unsafe actor_set_rotation as ^
   `(ActorClass self)' => { withActorClass* `self',
                            cFromEnum `RotateAxis',
                            `Double',
                            `Float',
                            `Float',
                            `Float' } -> `()' #}


-- | Sets the rotation angle of self around the Z axis using the
-- | center point specified as a compass point. For example to rotate
-- | such that the center of the actor remains static you can use
-- | 'GravityCenter'. If the actor changes size the center
-- | point will move accordingly.
--
-- [@self@] an Actor
--
-- [@angle@] the angle of rotation
--
-- [@gravity@] the center point of the rotation
--
-- * Since 1.0
--
{# fun unsafe actor_set_z_rotation_from_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', cFromEnum `Gravity' } -> `()' #}

-- TODO: Return something else, such as (angle, (x,y,z)) or anything not (angle,x,y,z)?
-- | Retrieves the angle and center of rotation on the given axis, set
--   using 'actorSetRotation'.
--
-- [@self@] an Actor
--
-- [@axis@] the axis of rotation
--
-- [@Return@] (&#x03B8;, X,Y,Z) the angle of rotation and X, Y, Z
-- coordinates of the center of rotation
--
-- * Since 0.8
--
{# fun unsafe actor_get_rotation as ^
   `(ActorClass self)' => { withActorClass* `self',
                            cFromEnum `RotateAxis',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `Double' #}


-- | Retrieves the center for the rotation around the Z axis as a
--   compass direction. If the center was specified in pixels or units
--   this will return 'GravityNone'.
--
-- [@self@] An Actor
--
-- [@Returns@] the Z rotation center
--
-- * Since 1.0
--
{# fun unsafe actor_get_z_rotation_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}

-- | Checks whether any rotation is applied to the actor.
--
-- [@self@] an Actor
--
-- [@Returns@] @True@ if the actor is rotated.
--
-- * Since 0.6
--
{# fun unsafe actor_is_rotated as ^ `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

-- | Sets the actor's opacity, with zero being completely transparent
--   and 255 (0xff) being fully opaque.
--
-- [@self@] An Actor
--
-- [@opacity@] New opacity value for the actor
--
{# fun unsafe actor_set_opacity as ^
   `(ActorClass self)' => { withActorClass* `self', `Word8' } -> `()' #}

-- | Retrieves the opacity value of an actor, as set by 'actorSetOpacity'.
--
-- For retrieving the absolute opacity of the actor inside a paint
-- virtual function, see actorGetPaintOpacity'.
--
-- [@self@] an Actor
--
-- [@Returns@] the opacity of the actor
--
{# fun unsafe actor_get_opacity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Word8' #}
actorOpacity :: (ActorClass self) => Attr self Word8
actorOpacity = newAttr actorGetOpacity actorSetOpacity

--CHECKME: setname, Bother passing in maybe, or just use String?
--CHECKME: OK to pass null?
--{# fun unsafe actor_set_name as ^
--   `(ActorClass self)' => { withActorClass* `self', `Maybe String' } -> `()' #}
-- | Sets the given name to self. The name can be used to identify an Actor.
--
-- [@self@] An Actor
--
-- [@name@] Textual tag to apply to actor
--
actorSetName :: (ActorClass self) => self -> Maybe String -> IO ()
actorSetName self mstr = let func = {# call unsafe actor_set_name #}
                         in withActorClass self $ \aPtr ->
                             case mstr of
                              Just str -> withCString str $ \strPtr -> func (castPtr aPtr) strPtr
                              Prelude.Nothing -> func (castPtr aPtr) nullPtr

-- | Retrieves the name of self.
--
-- [@self@] An Actor
--
-- [@Returns@] @Just@ the name of the actor, or @Nothing@
--
actorGetName :: (ActorClass self) => self -> IO (Maybe String)
actorGetName self = let func = {# call unsafe actor_get_name #}
                    in withActorClass self $ \aPtr ->
                        func aPtr >>= maybeNullString

actorName :: (ActorClass self) => Attr self (Maybe String)
actorName = newAttr actorGetName actorSetName

-- | Retrieves the unique id for self.
--
-- [@self@] An Actor
--
-- [@Returns@] Globally unique value for this object instance.
--
-- Since 0.6
--
{# fun unsafe actor_get_gid as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Word32' #}

actorGid :: (ActorClass self) => ReadAttr self Word32
actorGid = readAttr actorGetGid

-- | Sets clip area for self. The clip area is always computed from the
--   upper left corner of the actor, even if the anchor point is set
--   otherwise.
--
-- [@self@] An Actor
--
-- [@xoff@] X offset of the clip rectangle
--
-- [@yoff@] Y offset of the clip rectangle
--
-- [@width@] Width of the clip rectangle
--
-- [@height@] Height of the clip rectangle
--
-- * Since 0.6
--
{# fun unsafe actor_set_clip as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float', `Float', `Float' } -> `()' #}


-- |  Gets the clip area for self, if any is set
--
-- [@self@] an Actor
--
-- [@Returns@] (xoff, yoff, width, height)
--
-- * Since 0.6
--
{# fun unsafe actor_get_clip as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv* } -> `()' #}

-- | Determines whether the actor has a clip area set or not.
--
-- [@self@] an Actor
--
-- [@Returns@] @True@ if the actor has a clip area set.
--
-- * Since 0.1.1
--
{# fun unsafe actor_has_clip as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

-- | Removes clip area from self.
{# fun unsafe actor_remove_clip as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `()' #}

-- | Sets the parent of self to parent. The opposite function is
--  'actorUnparent'.
--
-- This function should not be used by applications, but by custom container actor subclasses.
--
-- [@self@] An Actor
--
-- [@parent@] A new Actor parent
--
{# fun unsafe actor_set_parent as ^
   `(ActorClass child, ActorClass parent)' => { withActorClass* `child', withActorClass* `parent' } -> `()' #}

-- | Retrieves the parent of self.
{# fun unsafe actor_get_parent as ^
   `(ActorClass child)' => { withActorClass* `child' } -> `Actor' newActor* #}
--actorParent :: (ActorClass self, ActorClass parent) => Attr self parent
--actorParent = newAttr actorGetPartent actorSetParent


-- | Removes the parent of self.
--
-- This function should not be used in applications. It should be
-- called by implementations of container actors, to dissociate a
-- child from the container.
--
-- [@self@] an Actor
--
-- * Since 0.1.1
--
{# fun unsafe actor_unparent as ^
   `(ActorClass child)' => { withActorClass* `child' } -> `()' #}


-- | This function resets the parent actor of self. It is logically
--   equivalent to calling 'actorUnparent' and 'actorSetParent', but
--   more efficiently implemented, ensures the child is not finalized
--   when unparented, and emits the parent-set signal only one time.
--
-- [@self@] an Actor
--
-- [@new_parent@] the new Actor parent
--
-- * Since 0.2
--
{# fun unsafe actor_reparent as ^
   `(ActorClass child, ActorClass newparent)' => { withActorClass* `child', withActorClass* `newparent' } -> `()' #}


-- | Puts self above below.
--
-- Both actors must have the same parent.
--
-- This function is the equivalent of 'containerRaiseChild'.
--
{# fun unsafe actor_raise as ^
   `(ActorClass self, ActorClass below)' => { withActorClass* `self', withActorClass* `below' } -> `()' #}

-- | Puts self below above.
--
-- Both actors must have the same parent.
--
-- This function is the equivalent of 'containerLowerChild'.
--
{# fun unsafe actor_lower as ^
   `(ActorClass self, ActorClass below)' => { withActorClass* `self', withActorClass* `below' } -> `()' #}

-- | Raises self to the top.
--
-- This function calls 'actorRaise' internally.
--
{# fun unsafe actor_raise_top as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `()' #}

-- | Lowers self to the bottom.
--
-- This function calls 'actorLower' internally.
--
{# fun unsafe actor_lower_bottom as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `()' #}


--TODO: Make Maybe, since can be null
-- | Retrieves the Stage where an actor is contained.
--
-- [@actor@]  an Actor
--
-- [@Returns@]
--
-- the @Just@ stage containing the actor, or @Nothing@.
--
-- * Since 0.8
--
actorGetStage :: (ActorClass self) => self -> IO (Maybe Stage)
actorGetStage self = withActorClass self $ \aPtr -> do
                       stgPtr <- {# call unsafe actor_get_stage #} aPtr
                       if stgPtr == nullPtr
                         then return Prelude.Nothing
                         else newStage stgPtr >>= return . Just

-- {# fun unsafe actor_get_stage as ^
--   `(ActorClass child)' => { withActorClass* `child' } -> `Stage' newStage* #}




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


