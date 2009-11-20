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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES csrc/clutter-macros.c #-}

#include <clutter/clutter.h>
#include <clutter-macros.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.Actor (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'Group'
-- |           +----'Rectangle'
-- |           +----'Texture'
-- |           +----'Clone'
-- |           +----'Text'
-- @

-- * Methods,
  actorIsRealized,
  actorIsMapped,
  actorIsVisible,
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
  actorSetShaderParamFloat,
  actorSetShaderParamInt,
  actorGrabKeyFocus,
  actorGetPangoContext,
  actorCreatePangoContext,
  actorCreatePangoLayout,
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
  actorAllocation,
  actorAnchorGravity,
  actorAnchorX,
  actorAnchorY,
  actorClip,
  actorClipToAllocation,
  actorDepth,
  actorFixedPositionSet,
  actorFixedX,
  actorFixedY,
--actorHasClip,
  actorHeight,
  actorMapped,
  actorMinHeight,
  actorMinHeightSet,
  actorMinWidth,
  actorMinWidthSet,
  actorName,
  actorNaturalHeight,
  actorNaturalHeightSet,
  actorNaturalWidth,
  actorNaturalWidthSet,
  actorOpacity,
  actorReactive,
  actorRealized,
  actorRequestMode,
  actorRotationAngleX,
  actorRotationAngleY,
  actorRotationAngleZ,
  actorRotationCenterX,
  actorRotationCenterY,
  actorRotationCenterZ,
  actorRotationCenterZGravity,
  actorScaleCenterX,
  actorScaleCenterY,
  actorScaleGravity,
  actorScaleX,
  actorScaleY,
  actorShowOnSetParent,
  actorVisible,
  actorWidth,
  actorX,
  actorY,

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
{# import Graphics.UI.Clutter.GValue #}
{# import Graphics.UI.Clutter.Utility #}
{# import Graphics.UI.Clutter.Signals #}

--FIXME: should I do something about clutter/prelude conflicts?
import Prelude hiding (show)

import C2HS
import Foreign
import Foreign.Ptr
import Data.IORef
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.Signals

--TODO: Export pango from types so you don't need this
import Graphics.UI.Gtk.Types (PangoContext, PangoLayoutRaw, mkPangoLayoutRaw)
import Graphics.UI.Gtk.Pango.Types
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Attributes
import Graphics.UI.Gtk.Pango.Enums (EllipsizeMode)


{# fun unsafe actor_is_mapped as ^ `(ActorClass actor)' => { withActorClass* `actor' } -> `Bool' #}

{# fun unsafe actor_is_realized as ^ `(ActorClass actor)' => { withActorClass* `actor' } -> `Bool' #}


{# fun unsafe actor_is_visible as ^ `(ActorClass actor)' => { withActorClass* `actor' } -> `Bool' #}


--FIXME: A lot of these need to be marked as safe, not unsafe for callbacks to work

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


-- | Calculates the transformed coordinates of the four corners of the
--   actor in the plane of ancestor. The returned vertices relate to
--   the 'ActorBox' coordinates as follows:
--
-- * [(x1, y1), (x2, y1), (x1, y2),  (x2, y2)]
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

--CHECKME: setname, Bother passing in maybe, or just use String?
--CHECKME: OK to pass null?
-- | Sets the given name to self. The name can be used to identify an Actor.
--
-- [@self@] An Actor
--
-- [@name@] Textual tag to apply to actor
--
{# fun unsafe actor_set_name as ^
    `(ActorClass self)' => { withActorClass* `self', withMaybeString* `Maybe String' } -> `()' #}

-- | Retrieves the name of self.
--
-- [@self@] An Actor
--
-- [@Returns@] @Just@ the name of the actor, or @Nothing@
--
{# fun unsafe actor_get_name as ^ `(ActorClass self)' =>
    { withActorClass* `self' } -> `Maybe String' maybeString* #}

-- | Retrieves the unique id for self.
--
-- [@self@] An Actor
--
-- [@Returns@] Globally unique value for this object instance.
--
-- Since 0.6
--
{# fun unsafe actor_get_gid as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `GID' cIntConv #}

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
{# fun unsafe actor_get_stage as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Maybe Stage' maybeNewStage* #}


-- | Retrieves the depth of /self/.
--
-- [@self@] an Actor
--
-- [@Returns@] the depth of the actor
--
{# fun unsafe actor_get_depth as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Float' #}

-- | Sets the Z coordinate of self to depth.
--
-- The unit used by depth is dependant on the perspective setup. See
-- also 'stageSetPerspective'.
--
-- [@self@] an Actor
--
-- [@depth@] Z co-ord
--
{# fun unsafe actor_set_depth as ^
   `(ActorClass self)' => { withActorClass* `self', `Float'} -> `()' #}

-- | Scales an actor with the given factors. The scaling is relative
--   to the scale center and the anchor point. The scale center is
--   unchanged by this function and defaults to 0,0.
--
-- [@self@] An Actor
--
-- [@scale_x@] factor to scale actor by horizontally.
--
-- [@scale_y@] factor to scale actor by vertically.
--
-- * Since 0.2
--
{# fun unsafe actor_set_scale as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', `Double'} -> `()' #}

-- | Scales an actor with the given factors around the given center
--   point. The center point is specified in pixels relative to the
--   anchor point (usually the top left corner of the actor).
--
-- [@self@] An Actor
--
-- [@scale_x@] factor to scale actor by horizontally.
--
-- [@scale_y@] factor to scale actor by vertically.
--
-- [@center_x@] X coordinate of the center of the scale.
--
-- [@center_y@] Y coordinate of the center of the scale
--
-- * Since 1.0
--
{# fun unsafe actor_set_scale_full as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', `Double', `Float', `Float' } -> `()' #}

-- | Scales an actor with the given factors around the given center
-- point. The center point is specified as one of the compass
-- directions in 'Gravity'. For example, setting it to north will
-- cause the top of the actor to remain unchanged and the rest of the
-- actor to expand left, right and downwards.
--
-- [@self@] An Actor
--
-- [@scale_x@] factor to scale actor by horizontally.
--
-- [@scale_y@] factor to scale actor by vertically.
--
-- [@gravity@] the location of the scale center expressed as a compass direction.
--
-- * Since 1.0
--
{# fun unsafe actor_set_scale_with_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', `Double', `Double', cFromEnum `Gravity'} -> `()' #}

-- | Retrieves an actors scale factors.
--
-- * Since 0.2
--
{# fun unsafe actor_get_scale as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv* } -> `()' #}


-- | Retrieves the scale center coordinate in pixels relative to the top
-- left corner of the actor. If the scale center was specified using a
-- 'Gravity' this will calculate the pixel offset using the
-- current size of the actor.
--
-- [@self@] an Actor
--
-- [@Returns@] (center_x, center_y)
--
-- * Since 0.2
--
{# fun unsafe actor_get_scale_center as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv* } -> `()' #}

-- | Retrieves the scale center as a compass direction. If the scale
--   center was specified in pixels or units this will return
--   'GravityNone'.
--
-- [@self@] An Actor
--
-- [@Returns@] the scale gravity
--
-- * Since 1.0
--
{# fun unsafe actor_get_scale_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}

-- | Checks whether the actor is scaled in either dimension.
--
-- [@self@] an Actor
--
-- [@Returns@] @True@ if the actor is scaled.
--
-- * Since 0.6
--
{# fun unsafe actor_is_scaled as ^
   `(ActorClass self)' => { withActorClass* `self'} -> `Bool' #}

-- | Transforms point in coordinates relative to the actor into
--   screen-relative coordinates with the current actor transformation
--   (i.e. scale, rotation, etc)
--
-- [@self@] An Actor
--
-- [@point@] A point as 'Vertex'
--
-- [@Returns@] The translated 'Vertex'
--
-- * Since 0.4
--
{# fun unsafe actor_apply_transform_to_point as ^
   `(ActorClass self)' => { withActorClass* `self',
                            withVertex* `Vertex',
                            alloca- `Vertex' peek*
                          } -> `()' #}


--CHECKME: unsafe?

-- | This function translates screen coordinates (x, y) to coordinates
-- relative to the actor. For example, it can be used to translate
-- screen events from global screen coordinates into actor-local
-- coordinates.
--
-- The conversion can fail, notably if the transform stack results in
-- the actor being projected on the screen as a mere line.
--
-- The conversion should not be expected to be pixel-perfect due to
-- the nature of the operation. In general the error grows when the
-- skewing of the actor rectangle on screen increases.
--
-- Note: This function is fairly computationally intensive.
--
-- Note: This function only works when the allocation is up-to-date,
-- i.e. inside of paint()
--
-- [@self@] An Actor
--
-- [@x@] x screen coordinate of the point to unproject.
--
-- [@y@] y screen coordinate of the point to unproject.
--
-- [@Returns@] (@True@ if conversion was successful, unprojected x
-- coordinance, unprojected y coordinance)
--
-- * Since 0.6
--
{# fun unsafe actor_transform_stage_point as ^
   `(ActorClass a)' => { withActorClass* `a',
                         `Float',
                         `Float',
                         alloca- `Float' peekFloatConv*,
                         alloca- `Float' peekFloatConv* } ->
                         `Bool' #}




-- | Transforms point in coordinates relative to the actor into
--   ancestor-relative coordinates using the relevant transform stack
--   (i.e. scale, rotation, etc).
--
-- If ancestor is @Nothing@ the ancestor will be the Stage. In this case,
-- the coordinates returned will be the coordinates on the stage
-- before the projection is applied. This is different from the
-- behaviour of 'actorApplyTransformToPoint'.
--
-- * Since 0.6
--
actorApplyRelativeTransformToPoint :: (ActorClass self, ActorClass ancestor) =>
                                      self          -- ^ An Actor
                                   -> Maybe ancestor
                                   -> Vertex        -- ^ A point as a 'Vertex'
                                   -> IO Vertex     -- ^ The translated 'Vertex'
actorApplyRelativeTransformToPoint self ancestor point =
    let func = {# call unsafe actor_apply_relative_transform_to_point #}
    in withActorClass self $ \selfPtr ->
         withVertex point $ \ptPtr -> do
           alloca $ \newVertPtr ->
             case ancestor of
               Prelude.Nothing -> func selfPtr nullPtr ptPtr newVertPtr >> peek newVertPtr
               Just ancActor -> withActorClass ancActor $ \ancPtr ->
                                      func selfPtr ancPtr ptPtr newVertPtr >> peek newVertPtr

{-
{# fun unsafe actor_apply_relative_transform_to_point as ^
   `(ActorClass self, ActorClass ancestor)' => { withActorClass* `self',
                                                 withActorClass* `ancestor',
                                                 withVertex* `Vertex',
                                                 alloca- `Vertex' peek*
                                               } -> `()' #}
-}


-- | Gets the absolute position of an actor, in pixels relative to the
--   stage.
--
-- [@self@] An Actor
--
-- [@Returns@] (x coordinate, y coordinate)
--
-- * Since 0.8
--
{# fun unsafe actor_get_transformed_position as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*
                          } -> `()' #}


--TODO: Properly format "Note" in doc
-- | Gets the absolute size of an actor in pixels, taking into account
--   the scaling factors.
--
-- If the actor has a valid allocation, the allocated size will be
-- used. If the actor has not a valid allocation then the preferred
-- size will be transformed and returned.
--
-- If you want the transformed allocation, see
-- 'actorGetAbsAllocationVertices' instead.
--
-- * Note
--
-- When the actor (or one of its ancestors) is rotated around the X or
-- Y axis, it no longer appears as on the stage as a rectangle, but as
-- a generic quadrangle; in that case this function returns the size
-- of the smallest rectangle that encapsulates the entire quad. Please
-- note that in this case no assumptions can be made about the
-- relative position of this envelope to the absolute position of the
-- actor, as returned by 'actorGetTransformedPosition'; if you need
-- this information, you need to use 'actorGetAbsAllocationVertices'
-- to get the coords of the actual quadrangle.
--
-- [@self@] An Actor
--
-- [@Returns@] (width, height)
--
-- * Since 0.8
--
{# fun unsafe actor_get_transformed_size as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Double' peekFloatConv*,
                            alloca- `Double' peekFloatConv* } -> `()' #}


-- | Retrieves the absolute opacity of the actor, as it appears on the
--   stage.
--
-- This function traverses the hierarchy chain and composites the
-- opacity of the actor with that of its parents.
--
-- This function is intended for subclasses to use in the paint
-- virtual function, to paint themselves with the correct opacity.
--
-- [@self@] An Actor
--
-- [@Returns@] The actor opacity value.
--
-- * Since 0.8
--
{# fun unsafe actor_get_paint_opacity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Word8' #}


-- | Retrieves the 'paint' visibility of an actor recursively checking
--   for non visible parents.
--
-- This is by definition the same as 'actorIsMapped'.
--
-- [@self@] An Actor
--
-- [@Returns@] @True@ if the actor is visibile and will be painted.
--
-- * Since 0.8.4
--
{# fun unsafe actor_get_paint_visibility as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}


-- | Calculates the transformed screen coordinates of the four corners
--   of the actor; the returned vertices relate to the 'ActorBox'
--   coordinates as follows:
--
--  [(x1, y1), (x2, y1),  (x1, y2),  (x2, y2)]
--
-- * Since 0.4
--
actorGetAbsAllocationVertices  :: (ActorClass self) => self -> IO [Vertex]
actorGetAbsAllocationVertices self = let func = {# call unsafe actor_get_abs_allocation_vertices #}
                                     in withActorClass self $ \selfPtr ->
                                          allocaArray 4 $ \vsPtr -> do
                                            func selfPtr vsPtr
                                            peekArray 4 vsPtr

{-
{# fun unsafe actor_get_transformation_matrix as ^ `(ActorClass self)' =>
{ withActorClass* `self', CoglMatrix } -> `()' #}
-}


-- | Sets actor as reactive. Reactive actors will receive events.
--
-- [@actor@] an Actor
--
-- [@reactive@] whether the actor should be reactive to events
--
-- * Since 0.6
--
{# fun unsafe actor_set_reactive as ^
   `(ActorClass self)' => { withActorClass* `self', `Bool' } -> `()' #}


-- | Checks whether actor is marked as reactive.
--
-- [@actor@] an Actor
--
-- [@Returns@] @True@ if the actor is reactive
--
-- * Since 0.6
--
{# fun unsafe actor_get_reactive as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

{# fun unsafe actor_set_shader as ^
   `(ActorClass self)' => { withActorClass* `self', withMaybeShader* `Maybe Shader' } -> `()' #}

--CHECKME: Clutter doc doesn't say returns null, but since you can set null, I assume this works
-- | Queries the currently set ClutterShader on self.
--
-- * Since 0.6
--
{# fun unsafe actor_get_shader as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Maybe Shader' maybeNewShader* #}

--{# fun unsafe actor_set_shader_param as ^


-- | Sets the value for a named float parameter of the shader applied
--   to actor.
--
-- [@self@] an Actor
--
-- [@param@] the name of the parameter
--
-- [@value@] the value of the parameter
--
-- * Since 0.8
--
{# fun unsafe actor_set_shader_param_float as ^
   `(ActorClass self)' => { withActorClass* `self', `String', `Float' } -> `()' #}

-- | Sets the value for a named int parameter of the shader applied to
--   actor.
--
-- [@self@] an Actor
--
-- [@param@] the name of the parameter
--
-- [@value@] the value of the parameter
--
-- * Since 0.8
--
{# fun unsafe actor_set_shader_param_int as ^
   `(ActorClass self)' => { withActorClass* `self', `String', `Int' } -> `()' #}

--CHECKME: unsafe?
-- | Sets the key focus of the 'Stage' including self to this Actor.
--
-- * Since 1.0
--
{# fun unsafe actor_grab_key_focus as ^
   `(ActorClass a)' => { withActorClass* `a' } -> `()' #}

--register PangoContext as ptr for c2hs to be happy and not need casts
{#pointer *PangoContext as PangoContextPtr foreign -> PangoContext nocode #}
{#pointer *PangoLayout as PangoLayoutPtr foreign -> PangoLayoutRaw nocode #}

--TODO: I think I figured out the pango stuff in Text
{# fun unsafe actor_get_pango_context as ^
   `(ActorClass a)' => { withActorClass* `a' } -> `PangoContext' newPangoContext* #}
{# fun unsafe actor_create_pango_context as ^
   `(ActorClass a)' => { withActorClass* `a' } -> `PangoContext' newPangoContext* #}

--CHECKME: This madness with the pango stuff is probably wrong, as well as in Text
--the string business
actorCreatePangoLayout :: (ActorClass self) => self -> String -> IO PangoLayout
actorCreatePangoLayout act str = let func = {# call unsafe actor_create_pango_layout #}
                                 in withActorClass act $ \actPtr ->
                                     withCString str $ \strPtr -> do
                                       pl <- constructNewGObject mkPangoLayoutRaw (func actPtr strPtr)
                                       withPangoLayoutRaw pl $ \plptr -> do
                                         ps <- makeNewPangoString str
                                         psRef <- newIORef ps
                                         return (PangoLayout psRef pl)



-- | Checks whether self is being currently painted by a 'Clone'
--
-- This function is useful only inside the ::paint virtual function
-- implementations or within handlers for the "paint" signal
--
-- This function should not be used by applications
--
-- [@self@] an Actor
--
-- [@Returns@] @True@ if the Actor is currently being painted by a 'Clone', and @False@ otherwise
--
-- * Since 1.0
--
{# fun unsafe actor_is_in_clone_paint as ^
       `(ActorClass self)' => { withActorClass* `self' } -> `Bool' #}

-- | Sets an anchor point for self. The anchor point is a point in the
--   coordinate space of an actor to which the actor position within
--   its parent is relative; the default is (0, 0), i.e. the top-left
--   corner of the actor.
--
-- [@self@] an Actor
--
-- [@anchor_x@] X coordinate of the anchor point
--
-- [@anchor_y@] Y coordinate of the anchor point
--
-- * Since 0.6
--
{# fun unsafe actor_set_anchor_point as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}

-- | Gets the current anchor point of the actor in pixels.
--
-- [@self@] an Actor
--
-- [@Returns@] (X coordinate of the anchor point, Y coordinate of the anchor point)
--
-- * Since 0.6
--
{# fun unsafe actor_get_anchor_point as ^
   `(ActorClass self)' => { withActorClass* `self',
                            alloca- `Float' peekFloatConv*,
                            alloca- `Float' peekFloatConv*} -> `()' #}


-- | Sets an anchor point on the actor, based on the given gravity
--   (this is a convenience function wrapping 'actorSetAnchorPoint').
--
-- Since version 1.0 the anchor point will be stored as a gravity so
-- that if the actor changes size then the anchor point will move. For
-- example, if you set the anchor point to 'GravitySouthEast' and
-- later double the size of the actor, the anchor point will move to
-- the bottom right.
--
-- [@self@] an Actor
--
-- [@gravity@] 'Gravity'.
--
-- * Since 0.6
--
{# fun unsafe actor_set_anchor_point_from_gravity as ^
   `(ActorClass self)' => { withActorClass* `self', cFromEnum `Gravity' } -> `()' #}

-- | Retrieves the anchor position expressed as a 'Gravity'. If the
--   anchor point was specified using pixels or units this will return
--   'GravityNone'.
--
-- [@self@] a Actor
--
-- [@Returns@] the 'Gravity' used by the anchor point
--
-- * Since 1.0
--
{# fun unsafe actor_get_anchor_point_gravity as ^
   `(ActorClass self)' => { withActorClass* `self' } -> `Gravity' cToEnum #}

-- | Sets an anchor point for the actor, and adjusts the actor postion
--   so that the relative position of the actor toward its parent
--   remains the same.
--
-- [@self@] an Actor
--
-- [@anchor_x@] X coordinate of the anchor point
--
-- [@anchor_y@] Y coordinate of the anchor point
--
-- * Since 0.6
--
{# fun unsafe actor_move_anchor_point as ^
   `(ActorClass self)' => { withActorClass* `self', `Float', `Float' } -> `()' #}

-- | Sets an anchor point on the actor based on the given gravity,
--   adjusting the actor postion so that its relative position within
--   its parent remains unchanged.
--
-- Since version 1.0 the anchor point will be stored as a gravity so
-- that if the actor changes size then the anchor point will move. For
-- example, if you set the anchor point to 'GravitySouthEast' and
-- later double the size of the actor, the anchor point will move to
-- the bottom right.
--
-- [@self@] an Actor
--
-- [@gravity@] 'Gravity'.
--
-- * Since 0.6
--
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


-- Attributes

--CHECKME: actorGetAllocationBox right?

-- | The allocation for the actor, in pixels
--
-- This is property is read-only, but you might monitor it to know
-- when an actor moves or resizes
--
-- * Since 0.8
--
actorAllocation :: (ActorClass self) => ReadAttr self ActorBox
actorAllocation = readNamedAttr "allocation" actorGetAllocationBox

-- | The anchor point expressed as a 'Gravity'
--
-- Default value: 'GravityNone'
--
-- * Since 1.0
--
actorAnchorGravity :: (ActorClass self) => Attr self Gravity
actorAnchorGravity = newNamedAttr "anchor-gravity" actorGetAnchorPointGravity actorSetAnchorPointFromGravity

-- | The X coordinate of an actor's anchor point, relative to the
--   actor coordinate space, in pixels
--
-- * Default value: 0
--
-- * Since 0.8
--
actorAnchorX :: (ActorClass self) => Attr self Float
actorAnchorX = newAttrFromFloatProperty "anchor-x"


-- | The Y coordinate of an actor's anchor point, relative to the
--   actor coordinate space, in pixels
--
-- Default value: 0
--
-- * Since 0.8
--
actorAnchorY :: (ActorClass self) => Attr self Float
actorAnchorY = newAttrFromFloatProperty "anchor-y"

-- | The clip region for the actor, in actor-relative coordinates
--
-- Every part of the actor outside the clip region will not be painted
--
actorClip :: (ActorClass self) => Attr self Geometry
actorClip = newAttrFromBoxedStorableProperty "clip" gTypeGeometry


-- | Whether the clip region should track the allocated area of the actor.
--
-- This property is ignored if a clip area has been explicitly set
-- using 'actorSetClip'.
--
-- Default value: @False@
--
-- * Since 1.0
--
actorClipToAllocation :: (ActorClass self) => Attr self Bool
actorClipToAllocation = newAttrFromBoolProperty "clip-to-allocation"

-- | The position of the actor on the Z axis
--
-- Default value: 0
--
-- * Since 0.6
--
actorDepth :: (ActorClass self) => Attr self Float
actorDepth = newNamedAttr "depth" actorGetDepth actorSetDepth


-- | This flag controls whether the 'actorFixedX' and 'actorFixedY'
--   properties are used
--
-- Default value: @False@
--
-- * Since 0.8
--
actorFixedPositionSet :: (ActorClass self) => Attr self Bool
actorFixedPositionSet = newNamedAttr "fixed-position-set" actorGetFixedPositionSet actorSetFixedPositionSet


-- | The fixed X position of the actor in pixels.
--
-- Writing this property sets 'actorFixedPositionSet' property as
-- well, as a side effect
--
-- Default value: 0
--
-- * Since 0.8
--
actorFixedX :: (ActorClass self) => Attr self Float
actorFixedX = newAttrFromFloatProperty "fixed-x"


-- | The fixed Y position of the actor in pixels.
--
-- Writing this property sets the 'actorFixedPositionSet' property as
-- well, as a side effect
--
-- Default value: 0
--
-- * Since 0.8
--
actorFixedY :: (ActorClass self) => Attr self Float
actorFixedY = newAttrFromFloatProperty "fixed-y"


-- FIXME: Name conflicts with the actual function.
-- | Whether the actor has the "clip" property set or not
--
-- Default value: @False@
--
--actorHasClip :: (ActorClass self) => ReadAttr self Bool
--actorHasClip = readNamedAttr "has-clip" actorHasClip


-- | Height of the actor (in pixels). If written, forces the minimum
--   and natural size request of the actor to the given height. If
--   read, returns the allocated height if available, otherwise the
--   height request.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
actorHeight :: (ActorClass self) => Attr self Float
actorHeight = newNamedAttr "height" actorGetHeight actorSetHeight


-- | Whether the actor is mapped (will be painted when the stage to
--   which it belongs is mapped)
--
-- Default value: @False@
--
-- * Since 1.0
--
actorMapped :: (ActorClass self) => ReadAttr self Bool
actorMapped = readNamedAttr "mapped" actorIsMapped


-- | A forced minimum height request for the actor, in pixels
--
-- Writing this property sets the 'actorMinHeightSet' property as
-- well, as a side effect. This property overrides the usual height
-- request of the actor.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 0.8
--
actorMinHeight :: (ActorClass self) => Attr self Float
actorMinHeight = newAttrFromFloatProperty "min-height"


-- | This flag controls whether the 'actorMinHeight' property is used
--
-- Default value: @False@
--
-- Since 0.8
--
actorMinHeightSet :: (ActorClass self) => Attr self Bool
actorMinHeightSet = newAttrFromBoolProperty "min-height-set"


-- | A forced minimum width request for the actor, in pixels
--
-- Writing this property sets the 'actorMinWidthSet' property as well,
-- as a side effect.
--
-- This property overrides the usual width request of the actor.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 0.8
--
actorMinWidth :: (ActorClass self) => Attr self Float
actorMinWidth = newAttrFromFloatProperty "min-width"


-- | This flag controls whether the 'actorMinWidth' property is used
--
-- * Default value: @False@
--
-- * Since 0.8
--
actorMinWidthSet :: (ActorClass self) => Attr self Bool
actorMinWidthSet = newAttrFromBoolProperty "min-width"

-- | The name of the actor
--
-- Default value: @Nothing@
--
-- * Since 0.2
--
actorName :: (ActorClass self) => Attr self (Maybe String)
actorName = newNamedAttr "name" actorGetName actorSetName


-- | A forced natural height request for the actor, in pixels
--
-- Writing this property sets the 'actorNaturalHeightSet' property as
-- well, as a side effect. This property overrides the usual height
-- request of the actor
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 0.8
--
actorNaturalHeight :: (ActorClass self) => Attr self Float
actorNaturalHeight = newAttrFromFloatProperty "natural-height"

-- | This flag controls whether the 'actorNaturalHeight' property is
--   used
--
-- Default value: @False@
--
-- * Since 0.8
--
actorNaturalHeightSet :: (ActorClass self) => Attr self Bool
actorNaturalHeightSet = newAttrFromBoolProperty "natural-height-set"


-- | A forced natural width request for the actor, in pixels
--
-- Writing this property sets the 'actorNaturalWidthSet' property as
-- well, as a side effect. This property overrides the usual width
-- request of the actor
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Since 0.8
--
actorNaturalWidth :: (ActorClass self) => Attr self Float
actorNaturalWidth = newAttrFromFloatProperty "natural-width"


-- | This flag controls whether the 'actorNaturalWidth' property is
--   used
--
-- Default value: @False@
--
-- * Since 0.8
--
actorNaturalWidthSet :: (ActorClass self) => Attr self Bool
actorNaturalWidthSet = newAttrFromBoolProperty "natural-width-set"

-- | Opacity of the actor, between 0 (fully transparent) and 255
--   (fully opaque)
--
-- Default value: 255
--
actorOpacity :: (ActorClass self) => Attr self Word8
actorOpacity = newNamedAttr "opacity" actorGetOpacity actorSetOpacity


-- | Whether the actor is reactive to events or not
--
-- Only reactive actors will emit event-related signals
--
-- Default value: @False@
--
-- * Since 0.6
--
actorReactive :: (ActorClass self) => Attr self Bool
actorReactive = newNamedAttr "reactive" actorGetReactive actorSetReactive

-- | Whether the actor has been realized
--
-- Default value: @False@
--
-- * Since 1.0
--
actorRealized :: (ActorClass self) => ReadAttr self Bool
actorRealized = readNamedAttr "realized" actorIsRealized

-- | Request mode for the ClutterActor. The request mode determines
--   the type of geometry management used by the actor, either height
--   for width (the default) or width for height.
--
--  For actors implementing height for width, the parent container
--  should get the preferred width first, and then the preferred
--  height for that width.
--
-- For actors implementing width for height, the parent container
-- should get the preferred height first, and then the preferred width
-- for that height.
--
-- For instance:
-- TODO: Example
--
-- will retrieve the minimum and natural width and height depending on
-- the preferred request mode of the ClutterActor "child".
--
-- The 'actorGetPreferredSize' function will implement this check for
-- you.
--
-- Default value: 'RequestHeightForWidth'
--
-- * Since 0.8
--
actorRequestMode :: (ActorClass self) => Attr self RequestMode
actorRequestMode = newAttrFromEnumProperty "realized" gTypeRequestMode

-- | The rotation angle on the X axis
--
-- Default value: 0
--
-- * Since 0.6
--
actorRotationAngleX :: (ActorClass self) => Attr self Double
actorRotationAngleX = newAttrFromDoubleProperty "rotation-angle-x"


-- | The rotation angle on the Y axis
--
-- Default value: 0
--
-- * Since 0.6
--
actorRotationAngleY :: (ActorClass self) => Attr self Double
actorRotationAngleY = newAttrFromDoubleProperty "rotation-angle-y"


-- | The rotation angle on the Z axis
--
-- Default value: 0
--
-- * Since 0.6
--
actorRotationAngleZ :: (ActorClass self) => Attr self Double
actorRotationAngleZ = newAttrFromDoubleProperty "rotation-angle-z"


actorRotationCenterX :: (ActorClass self) => Attr self Vertex
actorRotationCenterX = newAttrFromBoxedStorableProperty "rotation-center-x" gTypeVertex

actorRotationCenterY :: (ActorClass self) => Attr self Vertex
actorRotationCenterY = newAttrFromBoxedStorableProperty "rotation-center-y" gTypeVertex

actorRotationCenterZ :: (ActorClass self) => Attr self Vertex
actorRotationCenterZ = newAttrFromBoxedStorableProperty "rotation-center-z" gTypeVertex


-- | The rotation center on the Z axis expressed as a 'Gravity'.
--
-- Default value: 'GravityNone'
--
-- * Since 1.0
--
actorRotationCenterZGravity :: (ActorClass self) => Attr self Gravity
actorRotationCenterZGravity = newAttrFromEnumProperty "rotation-center-z-gravity" gTypeGravity


-- | The horizontal center point for scaling
--
-- Default value: 0
--
-- * Since 1.0
--
actorScaleCenterX :: (ActorClass self) => Attr self Float
actorScaleCenterX = newAttrFromFloatProperty "scale-center-x"


-- | The vertical center point for scaling
--
-- Default value: 0
--
-- * Since 1.0
--
actorScaleCenterY :: (ActorClass self) => Attr self Float
actorScaleCenterY = newAttrFromFloatProperty "scale-center-y"


-- | The center point for scaling expressed as a 'Gravity'
--
-- Default value: 'GravityNone'
--
-- * Since 1.0
--
actorScaleGravity :: (ActorClass self) => Attr self Gravity
actorScaleGravity = newAttrFromEnumProperty "scale-gravity" gTypeGravity



-- | The horizontal scale of the actor
--
-- Allowed values: >= 0
--
-- Default value: 1
--
-- * Since 0.6
--
actorScaleX :: (ActorClass self) => Attr self Double
actorScaleX = newAttrFromDoubleProperty "scale-x"



-- | The vertical scale of the actor
--
-- Allowed values: >= 0
--
-- Default value: 1
--
-- * Since 0.6
--
actorScaleY :: (ActorClass self) => Attr self Double
actorScaleY = newAttrFromDoubleProperty "scale-y"



-- | If @True@, the actor is automatically shown when parented.
--
-- Calling 'actorHide' on an actor which has not been parented will
-- set this property to @False@ as a side effect.
--
-- Default value: @True@
--
-- * Since 0.8
--
actorShowOnSetParent :: (ActorClass self) => Attr self Bool
actorShowOnSetParent = newAttrFromBoolProperty "show-on-set-parent"


-- | Whether the actor is set to be visible or not
--
-- See also 'actorMapped'
--
-- Default value: @False@
--
actorVisible :: (ActorClass self) => Attr self Bool
actorVisible = newAttrFromBoolProperty "visible"

actorWidth :: (ActorClass self) => Attr self Float
actorWidth = newNamedAttr "width" actorGetWidth actorSetWidth





-- | X coordinate of the actor in pixels. If written, forces a fixed
--   position for the actor. If read, returns the fixed position if
--   any, otherwise the allocation if available, otherwise 0.
--
-- Default value: 0
--
actorX :: (ActorClass self) => Attr self Float
actorX = newNamedAttr "x" actorGetX actorSetX


-- | Y coordinate of the actor in pixels. If written, forces a fixed
--   position for the actor. If read, returns the fixed position if
--   any, otherwise the allocation if available, otherwise 0.
--
-- Default value: 0
--
actorY :: (ActorClass self) => Attr self Float
actorY = newNamedAttr "y" actorGetY actorSetY

--actorGeometry :: (ActorClass self) => Attr self Geometry
--actorGeometry = newAttr actorGetGeometry actorSetGeometry

--actorGid :: (ActorClass self) => ReadAttr self GID
--actorGid = readAttr actorGetGid

--actorParent :: (ActorClass self, ActorClass parent) => Attr self parent
--actorParent = newAttr actorGetPartent actorSetParent

--actorTransformedSize :: (ActorClass self) => ReadAttr self (Double, Double)
--actorTransformedSize = readAttr actorGetTransformedSize

--actorPaintOpacity :: (ActorClass self) => ReadAttr self Word8
--actorPaintOpacity = readAttr actorGetPaintOpacity

--actorPaintVisibility :: (ActorClass self) => ReadAttr self Bool
--actorPaintVisibility = readAttr actorGetPaintVisibility

--actorShader :: (ActorClass self) => Attr self (Maybe Shader)
--actorShader = newAttr actorGetShader actorSetShader

-- Signals

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

-- | The \"destroy\" signal is emitted when an actor is destroyed,
--   either by direct invocation of 'actorDestroy' or when the 'Group'
--   that contains the actor is destroyed.
destroy :: ActorClass self => Signal self (IO ())
destroy = Signal (connect_NONE__NONE "destroy")


onHide, afterHide :: ActorClass a => a -> IO () -> IO (ConnectId a)
onHide = connect_NONE__NONE "hide" False
afterHide = connect_NONE__NONE "hide" True


-- | The \"hide\" signal is emitted when an actor is no longer
--   rendered on the stage.
hide :: ActorClass self => Signal self (IO ())
hide = Signal (connect_NONE__NONE "hide")


onKeyFocusIn, afterKeyFocusIn :: ActorClass a => a -> IO () -> IO (ConnectId a)
onKeyFocusIn = connect_NONE__NONE "key_focus_in" False
afterKeyFocusIn = connect_NONE__NONE "key_focus_in" True

-- | The \"key-focus-in\" signal is emitted when actor recieves key focus.
--
-- * Since 0.6
--
keyFocusIn :: ActorClass self => Signal self (IO ())
keyFocusIn = Signal (connect_NONE__NONE "key_focus_in")


onKeyFocusOut, afterKeyFocusOut :: ActorClass a => a -> IO () -> IO (ConnectId a)
onKeyFocusOut = connect_NONE__NONE "key_focus_out" False
afterKeyFocusOut = connect_NONE__NONE "key_focus_out" True

-- | The \"key-focus-out\" signal is emitted when actor loses key focus.
--
-- * Since 0.6
--
keyFocusOut :: ActorClass self => Signal self (IO ())
keyFocusOut = Signal (connect_NONE__NONE "key_focus_out")


onPaint, afterPaint :: ActorClass a => a -> IO () -> IO (ConnectId a)
onPaint = connect_NONE__NONE "paint" False
afterPaint = connect_NONE__NONE "paint" True

-- | The \"paint\" signal is emitted each time an actor is being
--   painted.
--
-- Subclasses of Actor should override the class signal handler and paint themselves in that function.
--
--
-- It is possible to connect a handler to the \"paint\" signal in order to set up some custom aspect of a paint.
--
-- * Since 0.8
--
paint :: ActorClass self => Signal self (IO ())
paint = Signal (connect_NONE__NONE "paint")


onParentSet, afterParentSet :: ActorClass a => a -> (Actor -> IO ()) -> IO (ConnectId a)
onParentSet = connect_OBJECT__NONE "parent-set" False
afterParentSet = connect_OBJECT__NONE "parent-set" True

--FIXME: old_parent can be null
-- | This signal is emitted when the parent of the actor changes.
--
-- [@actor@] :
--
-- the object which received the signal
--
-- [@old_parent@] the previous parent of the actor, or NULL
--
-- * Since 0.2
--
parentSet :: ActorClass self => Signal self (Actor -> IO ())
parentSet = Signal (connect_OBJECT__NONE "parent")


onPick, afterPick :: ActorClass a => a -> (Color -> IO ()) -> IO (ConnectId a)
onPick = connect_BOXED__NONE "pick" peek False
afterPick = connect_BOXED__NONE "pick" peek True


-- | The ::pick signal is emitted each time an actor is being painted
--   in "pick mode". The pick mode is used to identify the actor
--   during the event handling phase, or by 'stageGetActorAtPos'. The
--   actor should paint its shape using the passed pick_color.
--
--   Subclasses of ClutterActor should override the class signal handler
--   and paint themselves in that function.
--
--   It is possible to connect a handler to the ::pick signal in order
--   to set up some custom aspect of a paint in pick mode.
--
-- [@actor@] the Actor that received the signal
--
-- [@color@] the 'Color' to be used when picking
--
-- * Since 1.0
--
pick :: ActorClass self => Signal self (Color -> IO ())
pick = Signal (connect_BOXED__NONE "pick" peek)


onQueueRedraw, afterQueueRedraw :: ActorClass a => a -> (Actor -> IO ()) -> IO (ConnectId a)
onQueueRedraw = connect_OBJECT__NONE "queue-redraw" False
afterQueueRedraw = connect_OBJECT__NONE "queue-redraw" True

queueRedraw :: ActorClass self => Signal self (Color -> IO ())
queueRedraw = Signal (connect_BOXED__NONE "queue-redraw" peek)


onRealize, afterRealize :: ActorClass a => a -> IO () -> IO (ConnectId a)
onRealize = connect_NONE__NONE "realize" False
afterRealize = connect_NONE__NONE "realize" True


-- | The \"realize\" signal is emitted each time an actor is being
--   realized.
--
-- * Since 0.8
--
realize :: ActorClass self => Signal self (IO ())
realize = Signal (connect_NONE__NONE "realize")

onShow, afterShow :: ActorClass a => a -> IO () -> IO (ConnectId a)
onShow = connect_NONE__NONE "show" False
afterShow = connect_NONE__NONE "show" True


-- | The \"show\" signal is emitted when an actor is visible and
--   rendered on the stage.
--
-- * Since 0.2
--
show :: ActorClass self => Signal self (IO ())
show = Signal (connect_NONE__NONE "show")

onUnrealize, afterUnrealize :: ActorClass a => a -> IO () -> IO (ConnectId a)
onUnrealize = connect_NONE__NONE "unrealize" False
afterUnrealize = connect_NONE__NONE "unrealize" True


-- | The \"unrealize\" signal is emitted each time an actor is being
--   unrealized.
--
-- * Since 0.8
--
unrealize :: ActorClass self => Signal self (IO ())
unrealize = Signal (connect_NONE__NONE "unrealize")


