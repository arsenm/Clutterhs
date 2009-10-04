-- -*-haskell-*-
--  Clutter BehaviourEllipse
--
--  Author : Matthew Arsenault
--
--  Created: 3 Oct 2009
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

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}

module Graphics.UI.Clutter.BehaviourEllipse (
                                             behaviourEllipseNew,
                                             behaviourEllipseSetCenter,
                                             behaviourEllipseGetCenter,
                                           --behaviourEllipseCenter,
                                             behaviourEllipseSetAngleStart,
                                             behaviourEllipseGetAngleStart,
                                             behaviourEllipseAngleStart,
                                             behaviourEllipseSetAngleEnd,
                                             behaviourEllipseGetAngleEnd,
                                             behaviourEllipseAngleEnd,

                                             behaviourEllipseSetAngleTilt,
                                             behaviourEllipseGetAngleTilt,
                                             --behaviourEllipseAngleTilt, TODO: Property for each axis

                                             behaviourEllipseSetHeight,
                                             behaviourEllipseGetHeight,
                                             behaviourEllipseHeight,

                                             behaviourEllipseSetWidth,
                                             behaviourEllipseGetWidth,
                                             behaviourEllipseWidth,

                                             behaviourEllipseSetTilt,
                                             behaviourEllipseGetTilt,
                                           --behaviourEllipseTilt,

                                             behaviourEllipseSetDirection,
                                             behaviourEllipseGetDirection,
                                             behaviourEllipseDirection
                                          ) where

{# import Graphics.UI.Clutter.Types #}

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes

{# fun unsafe behaviour_ellipse_new as ^
       { withAlpha* `Alpha',
         `Int',
         `Int',
         `Int',
         `Int',
         cFromEnum `RotateDirection',
         `Double',
         `Double'
         } -> `BehaviourDepth' newBehaviourDepth* #}

{# fun unsafe behaviour_ellipse_set_center as ^
       { withBehaviourEllipse* `BehaviourEllipse', `Int', `Int' } -> `()' #}

{# fun unsafe behaviour_ellipse_get_center as ^
       { withBehaviourEllipse* `BehaviourEllipse',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}

--TODO: Make this work
--behaviourEllipseCenter :: Attr BehaviourEllipse (Int, Int)
--behaviourEllipseCenter = newAttr behaviourGetCenter behaviourSetCenter

{# fun unsafe behaviour_ellipse_set_angle_start as ^
       { withBehaviourEllipse* `BehaviourEllipse', `Double' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_angle_start as ^
       { withBehaviourEllipse* `BehaviourEllipse' } -> `Double' #}
behaviourEllipseAngleStart :: Attr BehaviourEllipse Double
behaviourEllipseAngleStart = newAttr behaviourEllipseGetAngleStart behaviourEllipseSetAngleStart

{# fun unsafe behaviour_ellipse_set_angle_end as ^
       { withBehaviourEllipse* `BehaviourEllipse', `Double' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_angle_end as ^
       { withBehaviourEllipse* `BehaviourEllipse' } -> `Double' #}
behaviourEllipseAngleEnd :: Attr BehaviourEllipse Double
behaviourEllipseAngleEnd = newAttr behaviourEllipseGetAngleEnd behaviourEllipseSetAngleEnd

{# fun unsafe behaviour_ellipse_set_angle_tilt as ^
       { withBehaviourEllipse* `BehaviourEllipse', cFromEnum `RotateAxis', `Double' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_angle_tilt as ^
       { withBehaviourEllipse* `BehaviourEllipse', cFromEnum `RotateAxis' } -> `Double' #}
--TODO: Deal with the fact that you have the RotateAxis and stuff
--behaviourEllipseAngleTilt :: Attr BehaviourEllipse Double
--behaviourEllipseAngleTilt = newAttr behaviourGetAngleTilt behaviourSetAngleTilt

{# fun unsafe behaviour_ellipse_set_height as ^
       { withBehaviourEllipse* `BehaviourEllipse', `Int' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_height as ^
       { withBehaviourEllipse* `BehaviourEllipse' } -> `Int' #}
behaviourEllipseHeight :: Attr BehaviourEllipse Int
behaviourEllipseHeight = newAttr behaviourEllipseGetHeight behaviourEllipseSetHeight

{# fun unsafe behaviour_ellipse_set_width as ^
       { withBehaviourEllipse* `BehaviourEllipse', `Int' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_width as ^
       { withBehaviourEllipse* `BehaviourEllipse' } -> `Int' #}
behaviourEllipseWidth :: Attr BehaviourEllipse Int
behaviourEllipseWidth = newAttr behaviourEllipseGetWidth behaviourEllipseSetWidth

{# fun unsafe behaviour_ellipse_set_tilt as ^
       { withBehaviourEllipse* `BehaviourEllipse',
       `Double',
       `Double',
       `Double' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_tilt as ^
       { withBehaviourEllipse* `BehaviourEllipse',
         alloca- `Double' peekFloatConv* ,
         alloca- `Double' peekFloatConv*,
         alloca- `Double' peekFloatConv*
       } -> `()' #}
--behaviourEllipseTilt :: Attr BehaviourEllipse (Double, Double, Double)
--behaviourEllipseTilt = newAttr behaviourGetTilt behaviourSetTilt


{# fun unsafe behaviour_ellipse_set_direction as ^
       { withBehaviourEllipse* `BehaviourEllipse', cFromEnum `RotateDirection' } -> `()' #}
{# fun unsafe behaviour_ellipse_get_direction as ^
       { withBehaviourEllipse* `BehaviourEllipse' } -> `RotateDirection' cToEnum #}
behaviourEllipseDirection :: Attr BehaviourEllipse RotateDirection
behaviourEllipseDirection = newAttr behaviourEllipseGetDirection behaviourEllipseSetDirection

