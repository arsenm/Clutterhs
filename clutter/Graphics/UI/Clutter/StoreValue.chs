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
-- |
-- Maintainer  : arsenm2@rpi.edu
-- Stability   : experimental
-- Portability : portable (depends on GHC)
--

{-# LANGUAGE ForeignFunctionInterface,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleInstances,
             FunctionalDependencies,
             EmptyDataDecls                #-} -- If this is on next line, GHC panic

#include <clutter/clutter.h>
#include <glib.h>

{# context lib="clutter" prefix="clutter" #}

--FIXME: Overall this is very messy

--FIXME: Too much duplication, and unnecessary incompatability between
--2 different representations of GValue

--StoreValue in Gtk2hs was missing some pieces for color (which also
--must be different than boxed) that wouldn't make sense to put there,
--such as GV's for color.
--

module Graphics.UI.Clutter.StoreValue (
                                       AnimType(..),
                                       GenericValue(..),
                                       GenericValuePtr,
                                       valueSetGenericValue,
                                       valueGetGenericValue,
                                       GenericValueClass(..),
                                       withGenericValue,
                                       unsetGValue,
                                       unsetOneGVal,
                                       genericValuePtrGetType,
                                       allocaTypedGValue
                                      ) where

import C2HS
import Control.Monad (liftM)

import Control.Exception

{# import Graphics.UI.Clutter.Types #}
{# import qualified Graphics.UI.Clutter.GTypes #} as CGT

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GValue
import System.Glib.GValueTypes
import qualified System.Glib.GTypeConstants as GType
import System.Glib.Types
import System.Glib.GType

-- | A union with information about the currently stored type.
--
-- * Internally used by "Graphics.UI.Clutter.Animation".

{# pointer *GValue as GenericValuePtr -> GenericValue #}

-- most of this is duplicated in gtk2hs, and should be fixed

genericValuePtrGetType :: GenericValuePtr -> IO GType
genericValuePtrGetType gvPtr = {# get GValue->g_type #} gvPtr

--GenericValue is a wrapper around haskell types that can be put in a gvalue
--GValue is the actual C type. GenericValues get put into GValues
--This is mostly the same as in gtk2hs, but with clutter types included

--CHECKME: Types for char, uchar. Do we want them to actually be Int8, Word8?
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
                  | GVcolor   Color
              --- | GVunits   Units
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
  fromEnum ATcolor  = fromEnum  CGT.color
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
           | x == fromEnum CGT.color	 = ATcolor
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
valueSetGenericValue gvalue (GVcolor x)  =  do valueInit gvalue CGT.color
                                               valueSetColor gvalue x
--valueSetGenericValue gvalue (GVboxed x)   = valueSetPointer gvalue x

--valueSetGenericValue gvalue (GVenum x)    = do valueInit gvalue GType.enum
--                                               valueSetUInt    gvalue (fromIntegral x)


--    TMenum	-> liftM (GVenum . fromIntegral)  $ valueGetUInt    gvalue


valueGetGenericValue :: GValue -> IO GenericValue
valueGetGenericValue gvalue = do
  gtype <- valueGetType gvalue
  case cToEnum gtype of
    ATinvalid	-> throwIO $ AssertionFailed "valueGetGenericValue: invalid or unavailable value."
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



--interval get stuff wants initialized type. This is essentially allocaGValue
allocaTypedGValue :: GType -> (GenericValuePtr -> IO a) -> IO (GenericValue, a)
allocaTypedGValue gtype body =
  allocaBytes ({# sizeof GType #}+ 2* {# sizeof guint64 #}) $ \gvPtr -> do
    {# set GValue->g_type #} gvPtr gtype
    ret <- body gvPtr
    val <- valueGetGenericValue (GValue (castPtr gvPtr))
    {#call unsafe g_value_unset#} gvPtr
    return (val, ret)



mkGValueFromGenericValue :: GenericValue -> IO GenericValuePtr
mkGValueFromGenericValue gv = do cptr <- malloc
                                 poke cptr gv
                                 return cptr


freeGValue :: GenericValuePtr -> IO ()
freeGValue p = unsetGValue (castPtr p) >> free p

--get gvalue out: allocaGValue (in body use valueGetGenericValue), then

withGenericValue :: (GenericValueClass arg) => arg -> (GenericValuePtr -> IO a) -> IO a
withGenericValue gv = bracket (mkGValueFromGenericValue (toGenericValue gv)) freeGValue


--This madness is almost straight from Haskell wiki on AdvancedOverlap
-- I only halfway understand it. It should pick the GObjectClass
-- instance for any gobject, and the others for anything else.


-- Wrapper type for gvalues you want to use in Clutter with unsafe
--  extraction. This should never be needed directly, and the type for
--  the extraction should be constrained to be the proper type by
--  every function I think that needs this.
class GenericValueClass a where
  toGenericValue :: a -> GenericValue
  unsafeExtractGenericValue :: GenericValue -> a

class GenericValueClass' flag a where
  toGenericValue' :: flag -> a -> GenericValue
  unsafeExtractGenericValue' :: flag -> GenericValue -> a

instance (GVPred a flag, GenericValueClass' flag a) => GenericValueClass a where
  toGenericValue = toGenericValue' (undefined::flag)
  unsafeExtractGenericValue = unsafeExtractGenericValue' (undefined::flag)


-- overlapping instances are used only for GVPred
class GVPred a flag | a -> flag where {}

instance GVPred Word RegularGValue
instance GVPred Int RegularGValue
instance GVPred Word8 RegularGValue
instance GVPred Int8 RegularGValue
instance GVPred Bool RegularGValue
--Enum
--Flags
instance GVPred Float RegularGValue
instance GVPred Double RegularGValue
instance GVPred (Maybe String) RegularGValue
instance GVPred Color RegularGValue

data IsGObject
data RegularGValue

instance GenericValueClass' RegularGValue Int where
  toGenericValue'      _ x         = GVint x
  unsafeExtractGenericValue' _ (GVint x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue Word where
  toGenericValue'      _ x         = GVuint x
  unsafeExtractGenericValue' _ (GVuint x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue Word8 where
  toGenericValue'      _ x         = GVuchar x
  unsafeExtractGenericValue' _ (GVuchar x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue Int8 where
  toGenericValue'      _ x         = GVchar x
  unsafeExtractGenericValue' _ (GVchar x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue Bool where
  toGenericValue'      _ x         = GVboolean x
  unsafeExtractGenericValue' _ (GVboolean x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

-- TODO: Enum, Flags
  {-
instance GenericValueClass' RegularGValue Bool where
  toGenericValue'      _ x         = GVboolean x
  unsafeExtractGenericValue' _ (GVboolean x) = x
-}

instance GenericValueClass' RegularGValue Float where
  toGenericValue'      _ x         = GVfloat x
  unsafeExtractGenericValue' _ (GVfloat x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue Double where
  toGenericValue'      _ x         = GVdouble x
  unsafeExtractGenericValue' _ (GVdouble x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue (Maybe String) where
  toGenericValue'      _ x         = GVstring x
  unsafeExtractGenericValue' _ (GVstring x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance GenericValueClass' RegularGValue Color where
  toGenericValue'      _ x         = GVcolor x
  unsafeExtractGenericValue' _ (GVcolor x) = x
  unsafeExtractGenericValue' _ _ = typeMismatchError

instance (GObjectClass a) => GenericValueClass' IsGObject a where
  toGenericValue'      _ x         = GVobject (toGObject x)
  unsafeExtractGenericValue' _ (GVobject x) = unsafeCastGObject x
  unsafeExtractGenericValue' _ _ = typeMismatchError


--More madness from Haskell wiki. This is the part I really don't
-- understand.  "Used only if the other instances don't apply"
--instance TypeCast flag RegularGValue => GVPred a flag

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x


-- for every use of gvalues, the out type should be inferrable from
-- what you pass in, so this should never happen unless clutter is
-- doing horrible things inside
typeMismatchError =  error "unsafeExtractGenericValue: Type mismatch"

{# fun unsafe value_get_color as ^ { withGValue `GValue' } -> `Color' peek* #}
{# fun unsafe value_set_color as ^ { withGValue `GValue', withColor* `Color' } -> `()' #}



