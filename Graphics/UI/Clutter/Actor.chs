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

module Graphics.UI.Clutter.Actor (
                                  actorShow,
                                  actorShowAll,
                                  actorHide,
                                  actorHideAll,
                                  actorRealize,
                                  actorUnrealize,
                                  actorPaint,
                                  actorQueueRedraw,
                                  actorQueueRelayout,

                                  actorSetParent,
                                  actorSetPosition,
                                  actorGetPosition,
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
                                  actorY

                                 ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Foreign
import Foreign.Ptr
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties

--this file sure is useful.

--{#fun unsafe color_new as ^
--      {id `GUInt8', id `GUInt8', id `GUInt8', id `GUInt8'} -> `Color' mkColor* #}

--{# fun unsafe actor_show as ^
-- `(ActorClass o)' => {unActor `o'} -> `()' #}

{# fun unsafe actor_show as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_show_all as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_hide as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_hide_all as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_realize as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_unrealize as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_paint as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_queue_redraw as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_queue_relayout as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}

{# fun unsafe actor_map as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}
{# fun unsafe actor_unmap as ^ `(ActorClass o)' => {withActorClass* `o'} -> `()' #}


{-
actorShow :: ActorClass self => self -> IO ()
actorShow self =
  {# call unsafe actor_show #} (toActor self)
-}

{# fun unsafe actor_set_parent as ^
   `(ActorClass child, ActorClass parent)' => { withActorClass* `child', withActorClass* `parent' } -> `()' #}

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

