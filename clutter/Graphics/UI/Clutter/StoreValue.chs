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
             EmptyDataDecls             #-}

#include <clutter/clutter.h>
#include <glib.h>
#include "hsgvalue.h"

{# context lib="clutter" prefix="clutter" #}

--FIXME: Overall this is very messy

--FIXME: Too much duplication, and unnecessary incompatability between
--2 different representations of GValue

--StoreValue in Gtk2hs was missing some pieces for color (which also
--must be different than boxed) that wouldn't make sense to put there,
--such as GV's for color.
--

module Graphics.UI.Clutter.StoreValue (
                                       gValueToHaskellObj,
                                       gValueFromHaskellObj,
                                       newWithGValue,
                                       newWithGValueOut,
                                       newWithGValueOutRet,
                                       AnimType(..),
                                       GenericValue(..),
                                       GenericValuePtr,
                                       valueSetGenericValue,
                                       valueSetGenericValueNoInit,
                                       valueGetGenericValue,
                                       GenericValueClass(..),
                                       withGenericValue,
                                       unsetGValue,
                                       unsetOneGVal,
                                       genericValuePtrGetType,
                                       allocaTypedGValue,

                                       TypeCast,
                                       TypeCast',
                                       TypeCast''
                                      ) where

import C2HS
import Control.Monad (liftM)

import Control.Exception

{# import Graphics.UI.Clutter.Types #}
{# import qualified Graphics.UI.Clutter.GTypes #} as CGT

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


--Everything here is a horrible mess that needs to be fixed
valueSetGenericValueNoInit :: GValue -> GenericValue -> IO ()
valueSetGenericValueNoInit gvalue (GVuint x)    = valueSetUInt  gvalue x
valueSetGenericValueNoInit gvalue (GVint x)     = valueSetInt   gvalue x
--valueSetGenericValueNoInit gvalue (GVuchar x)   = valueSetUChar gvalue x
--valueSetGenericValueNoInit gvalue (GVchar x)    = valueSetChar  gvalue (cToEnum x)
valueSetGenericValueNoInit gvalue (GVboolean x) = valueSetBool  gvalue x
valueSetGenericValueNoInit gvalue (GVenum x)    = valueSetUInt  gvalue (fromIntegral x)
valueSetGenericValueNoInit gvalue (GVflags x)   = valueSetUInt  gvalue (fromIntegral x)
--valueSetGenericValueNoInit gvalue (GVpointer x) = valueSetPointer gvalue x
valueSetGenericValueNoInit gvalue (GVfloat x)   = valueSetFloat gvalue x
valueSetGenericValueNoInit gvalue (GVdouble x)  = valueSetDouble  gvalue x
valueSetGenericValueNoInit gvalue (GVstring x)  = valueSetMaybeString  gvalue x
valueSetGenericValueNoInit gvalue (GVobject x)  = valueSetGObject gvalue x
valueSetGenericValueNoInit gvalue (GVcolor x)   = valueSetColor  gvalue x



valueSetGenericValue :: GValue -> GenericValue -> IO ()
valueSetGenericValue gvalue (GVuint x)    = do valueInit gvalue GType.uint
                                               valueSetUInt gvalue x
valueSetGenericValue gvalue (GVint x)     = do valueInit gvalue GType.int
                                               valueSetInt  gvalue x
--valueSetGenericValue gvalue (GVuchar x)   = do valueInit gvalue GType.uchar
--                                               valueSetUChar gvalue x
--valueSetGenericValue gvalue (GVchar x)    = do valueInit gvalue GType.char
--                                               valueSetChar gvalue (cToEnum x)
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
--    ATuchar	-> liftM (GVuchar . cFromEnum)	  $ valueGetUChar   gvalue
--    ATchar	-> liftM (GVchar . cFromEnum)     $ valueGetChar    gvalue
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
    {# call unsafe g_value_unset #} gvPtr
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
--Enum FIXME: Doing enum this way isn't good enough
--Flags
instance GVPred Float RegularGValue
instance GVPred Double RegularGValue
instance GVPred (Maybe String) RegularGValue
instance GVPred Color RegularGValue
instance (Enum a) => GVPred a RegularGValue

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

instance (Enum a) => GenericValueClass' RegularGValue a where
  toGenericValue'      _ x         = GVenum (fromEnum x)
  unsafeExtractGenericValue' _ (GVenum x) = toEnum x
  unsafeExtractGenericValue' _ _ = typeMismatchError

-- TODO: Enum, Flags

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





--foreign import ccall unsafe "hsgvalue.h g_value_as_haskell_obj"
--  gvToHaskellObj :: Ptr GValue -> IO a

--foreign import ccall unsafe "hsgvalue.h applyGValueFunctionInt"
--  hsObjToGValue :: StablePtr a -> IO Int
-- testMagic :: Show a => a -> IO ()
-- testMagic a = do
--   putStrLn ("Original: " ++ show a)
--   sp <- newStablePtr a
--   i  <- hsObjToGValue sp
--   putStrLn ("Haskell side: " ++ show i)
--   freeStablePtr sp
--


-- Playing with these. Don't think I'll end up using it.


foreign import ccall unsafe "hsgvalue.h g_value_to_haskellobj"
  gValueToHaskellObj :: GValue -> IO (StablePtr a)



foreign import ccall safe "hsgvalue.h g_value_from_haskellobj"
  gValueFromHaskellObj :: GValue -> StablePtr a -> IO ()

-- in argument
newWithGValue :: GenericValueClass a => GType -> a -> (Ptr GValue -> IO b) -> IO b
newWithGValue gt v fun =
  allocaGValue $ \gv@(GValue gvp) -> do
    valueInit gv gt
    sp <- newStablePtr v
    gValueFromHaskellObj gv sp
    ret <- fun gvp
    freeStablePtr sp
    return ret


newWithGValueOutRet :: GenericValueClass a => GType -> a -> (Ptr GValue -> IO b) -> IO (a, b)
newWithGValueOutRet gt v fun =
  allocaGValue $ \gv@(GValue gvp) -> do
    valueInit gv gt
    sp <- newStablePtr v
    gValueFromHaskellObj gv sp
    ret <- fun gvp
    out <- gValueToHaskellObj gv
    outV <- deRefStablePtr out
    freeStablePtr sp
    freeStablePtr out
    return (outV, ret)


newWithGValueOut :: GenericValueClass a => GType -> a -> (Ptr GValue -> IO ()) -> IO a
newWithGValueOut gt v fun =
  allocaGValue $ \gv@(GValue gvp) -> do
    valueInit gv gt
    sp <- newStablePtr v
    gValueFromHaskellObj gv sp
    fun gvp
    out <- gValueToHaskellObj gv
    outV <- deRefStablePtr out
    freeStablePtr sp
    freeStablePtr out
    return outV

{-
-- more playing
moreWithGValueOut :: GenericValueClass a => GType -> a -> (Ptr GValue -> IO ()) -> IO a
moreWithGValueOut gt v f = do
  allocaGValue $ \gv@(GValue gvp) -> do
    valueInit gv gt

    f gv
-}


-- unsafe! This all assumes that you know the types from context. I
-- think this works in all reasonable cases.
class GValueClass a where
  gtype :: a -> GType
  gValueSet :: GValue -> a -> IO ()
  gValueGet :: GValue -> IO a

class GValueClass' flag a where
  gtype' :: flag -> a -> GType
  gValueSet' :: flag -> GValue -> a -> IO ()
  gValueGet' :: flag -> GValue -> IO a

instance (GVPred a flag, GValueClass' flag a) => GValueClass a where
  gtype     = gtype'     (undefined::flag)
  gValueSet = gValueSet' (undefined::flag)
  gValueGet = gValueGet' (undefined::flag)



instance GValueClass' RegularGValue Int where
  gtype'     _ _    = GType.int
  gValueSet' _ gv x = valueSetInt gv x
  gValueGet' _ gv   = valueGetInt gv

instance GValueClass' RegularGValue Double where
  gtype'     _ _    = GType.double
  gValueSet' _ gv x = valueSetDouble gv x
  gValueGet' _ gv   = valueGetDouble gv



withGValueClassOut :: GValueClass a => a -> (Ptr GValue -> IO ()) -> IO a
withGValueClassOut val fun = allocaGValue $ \gv@(GValue gvp) -> do
  valueInit gv (gtype val)
  gValueSet gv val
  fun gvp
  gValueGet gv





{-
setGValue2 :: GValue -> GType -> a -> IO ()
setGValue2 gv gt v = do
  -- TODO: GTypeFundamental
  case gt of
    GType.int    -> valueSetInt    gv v
    GType.uint   -> valueSetUInt   gv v
    GType.char   -> valueSetChar   gv (cToEnum v)
    GType.uchar  -> valueSetUChar  gv v
    GType.bool   -> valueSetBool   gv v
    GType.enum   -> valueSetUInt   gv (fromIntegral v)
    GType.flags  -> valueSetUInt   gv (fromIntegral v)
    GType.float  -> valueSetFloat  gv v
    GType.double -> valueSetDouble gv v
    GType.string -> valueSetMaybeString gv v
    GType.object -> valueSetGObject gv v
-}


{-
coercing would be unfortunate
getGValue :: GValue -> IO a
getGValue gv = do
  gtype <- valueGetType gv
  --TODO: GTypeFundamental
  case gtype of
    GType.int    -> valueGetInt         gv
    GType.uint   -> valueGetUInt        gv
    GType.char   -> valueGetChar        gv
    GType.uchar  -> valueGetUChar       gv
    GType.bool   -> valueGetBool        gv
    GType.enum   -> valueGetUInt        gv
    GType.flags  -> valueGetUInt        gv
    GType.float  -> valueGetFloat       gv
    GType.double -> valueGetDouble      gv
    GType.string -> valueGetMaybeString gv
    GType.object -> valueGetGObject     gv
-}


-- ???
--valueSetGenericValue gvalue (GVcolor x)  =  do valueInit gvalue CGT.color
--                                               valueSetColor gvalue x

--valueSetGenericValue gvalue (GVboxed x)   = valueSetPointer gvalue x
--valueSetGenericValue gvalue (GVpointer x) = valueSetPointer gvalue x
--valueSetGenericValue gvalue (GVenum x)    = do valueInit gvalue GType.enum
--                                               valueSetUInt    gvalue (fromIntegral x)


--    TMenum	-> liftM (GVenum . fromIntegral)  $ valueGetUInt    gvalue

