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

{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}


#include <clutter/clutter.h>
#include <glib.h>


--StoreValue in Gtk2hs was missing some pieces for color
--that wouldn't make sense to put there, such as GV's for color and units
--Instead of TreeModels, Animations and ClutterModels and so forth.
--Maybe do something about this later
module Graphics.UI.Clutter.StoreValue (
                                       AnimType(..),
                                       GenericValue(..),
                                       GenericValuePtr,
                                       valueSetGenericValue,
                                       valueGetGenericValue,
                                       GenericValueClass,
                                       toGenericValue,
                                       withGenericValue,
                                       unGValue,  --I don't remember why this is here.
                                       unsetGValue,
                                       unsetOneGVal
                                      ) where

import C2HS
import Control.Monad (liftM)

--TODO: New exceptions
import Control.OldException (throw, Exception(AssertionFailed))

import Control.Exception (bracket)

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.External #}
{# import Graphics.UI.Clutter.GValue #} (color, valueSetColor, valueGetColor)

import System.Glib.FFI
import System.Glib.GValue
import System.Glib.GValueTypes hiding (valueGetChar, valueGetUChar, valueSetUChar, valueSetChar)
import qualified System.Glib.GTypeConstants as GType
import System.Glib.Types
import System.Glib.GType

-- | A union with information about the currently stored type.
--
-- * Internally used by "Graphics.UI.Clutter.Animation".

{# pointer *GValue as GenericValuePtr -> GenericValue #}

--GenericValue is a wrapper around haskell types that can be put in a gvalue
--GValue is the actual C type. GenericValues get put into GValues

--CHECKME: Types for char, uchar. Do we want them to actually be Int8, Word8?
--that would also mean gtk2hs patch would need to change.
data GenericValue = GVuint    Word
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
-- It's also probably wrong.

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


valueSetGenericValue :: GValue -> GenericValue -> IO ()
valueSetGenericValue gvalue (GVuint x)    = do valueInit gvalue GType.uint
                                               valueSetUInt gvalue x
valueSetGenericValue gvalue (GVint x)     = do valueInit gvalue GType.int
                                               valueSetInt  gvalue x
valueSetGenericValue gvalue (GVuchar x)   = do valueInit gvalue GType.uchar
                                               valueSetUChar gvalue x
valueSetGenericValue gvalue (GVchar x)    = do valueInit gvalue GType.char
                                               valueSetChar gvalue (cToEnum x)
valueSetGenericValue gvalue (GVboolean x) = do valueInit gvalue GType.bool
                                               valueSetBool    gvalue x
valueSetGenericValue gvalue (GVenum x)    = do valueInit gvalue GType.enum
                                               valueSetUInt    gvalue (fromIntegral x)
valueSetGenericValue gvalue (GVflags x)   = do valueInit gvalue GType.flags
                                               valueSetUInt    gvalue (fromIntegral x)
--valueSetGenericValue gvalue (GVpointer x) = valueSetPointer gvalue x
valueSetGenericValue gvalue (GVfloat x)   = do valueInit gvalue GType.float
                                               valueSetFloat   gvalue x
valueSetGenericValue gvalue (GVdouble x)  = do valueInit gvalue GType.double
                                               valueSetDouble  gvalue x
valueSetGenericValue gvalue (GVstring x)  = do valueInit gvalue GType.string
                                               valueSetMaybeString  gvalue x
valueSetGenericValue gvalue (GVobject x)  = do valueInit gvalue GType.object
                                               valueSetGObject gvalue x
valueSetGenericValue gvalue (GVcolor x)  = do valueInit gvalue color
                                              valueSetColor gvalue x
--valueSetGenericValue gvalue (GVboxed x)   = valueSetPointer gvalue x

valueGetGenericValue :: GValue -> IO GenericValue
valueGetGenericValue gvalue = do
  gtype <- valueGetType gvalue
  case cToEnum gtype of
    ATinvalid	-> throw $ AssertionFailed "valueGetGenericValue: invalid or unavailable value."
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


--for folding along a list
--The list doesn't matter, just gets the length
unsetOneGVal :: Ptr GenericValue -> a -> IO (Ptr GenericValue)
unsetOneGVal i _ = {# call unsafe g_value_unset #} i >> return (advancePtr i 1)

unsetGValue :: Ptr GenericValue -> IO ()
unsetGValue p = {# call unsafe g_value_unset #} p

--CHECKME: Bad things?
instance Storable GenericValue where
    sizeOf _ = {# sizeof GValue #}
    alignment _ = alignment (undefined :: GType)
    peek p = let gv = GValue (castPtr p)
             in valueGetGenericValue gv
    poke p ut = let gv = GValue (castPtr p)
                in do
                  {# set GValue->g_type #} p (0 :: GType) --must be initialized to 0 or init will fail
                  valueSetGenericValue gv ut

mkGValueFromGenericValue :: GenericValue -> IO GenericValuePtr
mkGValueFromGenericValue gv = do cptr <- malloc
                                 poke cptr gv
                                 return cptr

freeGValue :: GenericValuePtr -> IO ()
freeGValue p = unsetGValue (castPtr p) >> free p

--get gvalue out: allocaGValue (in body use valueGetGenericValue), then

withGenericValue :: (GenericValueClass arg) => arg -> (GenericValuePtr -> IO a) -> IO a
withGenericValue gv = bracket (mkGValueFromGenericValue (toGenericValue gv)) freeGValue

class GenericValueClass a where
  toGenericValue :: a -> GenericValue
instance GenericValueClass Int where
  toGenericValue = GVint
instance GenericValueClass Word where
  toGenericValue = GVuint
instance GenericValueClass Float where
  toGenericValue = GVfloat
instance GenericValueClass Double where
  toGenericValue = GVdouble
instance GenericValueClass Int8 where
  toGenericValue = GVchar
instance GenericValueClass Word8 where
  toGenericValue = GVuchar
instance GenericValueClass Bool where
  toGenericValue = GVboolean
instance GenericValueClass String where
  toGenericValue = GVstring . Just

instance GenericValueClass Color where
  toGenericValue = GVcolor

--UndecidableInstances
--instance (GObjectClass obj) => GenericValueClass obj where
--  toGenericValue = GVobject . toGObject


unGValue :: GValue -> Ptr ()
unGValue (GValue a) = castPtr a

