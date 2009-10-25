-- -*-haskell-*-
--  GIMP Toolkit (GTK) StoreValue GenericValue
--  modified for use in Clutter
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--  Modified for Clutter: 25 Oct 2009
--
--  Copyright (c) 1999..2002 Axel Simon
--  Copyright (c) 2009 Matthew Arsenault
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- TODO: this module is deprecated and should be removed. The GenericValue
-- type is currently exposed to users and it should not be.
--
-- |
-- Maintainer  : arsenm2@rpi.edu
-- Stability   : experimental
-- Portability : portable (depends on GHC)
--

{-# LANGUAGE ForeignFunctionInterface #-}


#include <clutter/clutter.h>
#include <glib.h>


--StoreValue in Gtk2hs was missing some pieces for color
--that wouldn't make sense to put there, such as GV's for color and units
--Instead of TreeModels, Animations and ClutterModels and so forth.
--Maybe do something about this later
module Graphics.UI.Clutter.StoreValue (
                                       AnimType(..),
                                       ClutterGV(..),
                                       valueSetClutterGV,
                                       valueGetClutterGV
                                      ) where

import C2HS
import Control.Monad (liftM)

--TODO: New exceptions
import Control.OldException (throw, Exception(AssertionFailed))

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.External #}
{# import Graphics.UI.Clutter.GValue #}

import System.Glib.FFI
import System.Glib.GValue
import System.Glib.GValueTypes
import qualified System.Glib.GTypeConstants as GType
import System.Glib.Types
import System.Glib.GType

-- | A union with information about the currently stored type.
--
-- * Internally used by "Graphics.UI.Clutter.Animation".


--CHECKME: Types for char, uchar. Do we want them to actually be Int8, Word8?
--that would also mean gtk2hs patch would need to change.
data ClutterGV = GVuint    Word
	       | GVint     Int
	       | GVuchar   Word8
	       | GVchar    Int8
	       | GVboolean Bool
	       | GVenum    Int
	       | GVflags   Int
	   --- | GVpointer (Ptr ())
	       | GVfloat   Float
	       | GVdouble  Double
	       | GVstring  (Maybe String)
	       | GVobject  GObject
               | GVcolor  Color
           --- | GVunits  Units
	   --- | GVboxed   (Ptr ())

-- This is an enumeration of all GTypes that can be used in an Animation
--

data AnimType = ATinvalid
	      | ATuint
	      | ATint
              | ATuchar
              | ATchar
	      | ATbool
	      | ATenum
	      | ATflags
--	      | ATpointer
	      | ATfloat
	      | ATdouble
	      | ATstring
	      | ATobject
              | ATcolor
--	      | ATboxed


--workaround for some kind of c2hs limitation, TODO: Find a better solution
instance Enum AnimType where
  fromEnum ATinvalid = fromEnum GType.invalid
  fromEnum ATuint    = fromEnum GType.uint
  fromEnum ATint     = fromEnum GType.int
  fromEnum ATuchar   = fromEnum GType.uchar
  fromEnum ATchar    = fromEnum GType.char
  fromEnum ATbool    = fromEnum GType.bool
  fromEnum ATenum    = fromEnum GType.enum
  fromEnum ATflags   = fromEnum GType.flags
--fromEnum ATpointer = fromEnum GType.pointer
  fromEnum ATfloat   = fromEnum GType.float
  fromEnum ATdouble  = fromEnum GType.double
  fromEnum ATstring  = fromEnum GType.string
  fromEnum ATobject  = fromEnum GType.object
  fromEnum ATcolor  = fromEnum color
--fromEnum ATboxed   = fromEnum GType.boxed

  toEnum x | x == fromEnum GType.invalid = ATinvalid
           | x == fromEnum GType.uint    = ATuint
           | x == fromEnum GType.int	 = ATint
           | x == fromEnum GType.uchar   = ATuchar
           | x == fromEnum GType.char    = ATchar
           | x == fromEnum GType.bool    = ATbool
           | x == fromEnum GType.enum	 = ATenum
           | x == fromEnum GType.flags	 = ATflags
       --- | x == fromEnum GType.pointer = ATpointer
           | x == fromEnum GType.float	 = ATfloat
           | x == fromEnum GType.double	 = ATdouble
           | x == fromEnum GType.string	 = ATstring
           | x == fromEnum GType.object	 = ATobject
           | x == fromEnum color	 = ATcolor
       --- | x == fromEnum GType.boxed	 = ATboxed
           | otherwise	 = error "StoreValue.toEnum(AnimType): no dynamic types allowed."


valueSetClutterGV :: GValue -> ClutterGV -> IO ()
valueSetClutterGV gvalue (GVuint x)    = do valueInit gvalue GType.uint
                                            valueSetUInt gvalue x
valueSetClutterGV gvalue (GVint x)     = do valueInit gvalue GType.int
                                            valueSetInt  gvalue x
valueSetClutterGV gvalue (GVuchar x)   = valueSetUChar   gvalue x
valueSetClutterGV gvalue (GVchar x)    = valueSetChar    gvalue (cToEnum x)
valueSetClutterGV gvalue (GVboolean x) = do valueInit gvalue GType.bool
                                            valueSetBool    gvalue x
valueSetClutterGV gvalue (GVenum x)    = do valueInit gvalue GType.enum
                                            valueSetUInt    gvalue (fromIntegral x)
valueSetClutterGV gvalue (GVflags x)   = do valueInit gvalue GType.flags
                                            valueSetUInt    gvalue (fromIntegral x)
--valueSetClutterGV gvalue (GVpointer x) = valueSetPointer gvalue x
valueSetClutterGV gvalue (GVfloat x)   = do valueInit gvalue GType.float
                                            valueSetFloat   gvalue x
valueSetClutterGV gvalue (GVdouble x)  = do valueInit gvalue GType.double
                                            valueSetDouble  gvalue x
valueSetClutterGV gvalue (GVstring x)  = do valueInit gvalue GType.string
                                            valueSetMaybeString  gvalue x
valueSetClutterGV gvalue (GVobject x)  = do valueInit gvalue GType.object
                                            valueSetGObject gvalue x

valueSetClutterGV gvalue (GVcolor x)  = do valueInit gvalue color
                                           valueSetColor gvalue x
--valueSetClutterGV gvalue (GVboxed x)   = valueSetPointer gvalue x

valueGetClutterGV :: GValue -> IO ClutterGV
valueGetClutterGV gvalue = do
  gtype <- valueGetType gvalue
  case cToEnum gtype of
    ATinvalid	-> throw $ AssertionFailed "valueGetClutterGV: invalid or unavailable value."
    ATuint      -> liftM GVuint			  $ valueGetUInt    gvalue
    ATint	-> liftM GVint	                  $ valueGetInt	    gvalue
    ATuchar	-> liftM (GVuchar . cFromEnum)	  $ valueGetUChar   gvalue
    ATchar	-> liftM (GVchar . cFromEnum)     $ valueGetChar    gvalue
    ATbool	-> liftM GVboolean		  $ valueGetBool    gvalue
    ATenum	-> liftM (GVenum . cToEnum)       $ valueGetUInt    gvalue
    ATflags	-> liftM (GVflags . cToEnum)      $ valueGetUInt    gvalue
--  ATpointer	-> liftM GVpointer		  $ valueGetPointer gvalue
    ATfloat	-> liftM GVfloat		  $ valueGetFloat   gvalue
    ATdouble	-> liftM GVdouble		  $ valueGetDouble  gvalue
    ATstring	-> liftM GVstring		  $ valueGetMaybeString  gvalue
    ATobject	-> liftM GVobject		  $ valueGetGObject gvalue
    ATcolor	-> liftM GVcolor		  $ valueGetColor gvalue
--  ATboxed     -> liftM GVpointer		  $ valueGetPointer gvalue


instance Storable ClutterGV where
    sizeOf _ = {# sizeof GValue #}
    alignment _ = alignment (undefined :: GType)
    peek p = let gv = GValue (castPtr p)
             in valueGetClutterGV gv
    poke p ut = let gv = GValue (castPtr p)
                in valueSetClutterGV gv ut

