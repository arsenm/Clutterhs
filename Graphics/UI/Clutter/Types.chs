-- -*-haskell-*-
-- {-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}


module Graphics.UI.Clutter.Types (
                                  ClutterColor,
                                  mkClutterColor,
                                  unClutterColor
                                 ) where

--import C2HS
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr, unsafeForeignPtrToPtr,newForeignPtr_)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUChar)
import System.Glib.GType (GType, typeInstanceIsA)
import System.Glib.GObject

-- from gtk2hs
-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


{# pointer *ClutterColor as ClutterColor foreign newtype #}

instance Show ClutterColor where
    show (ClutterColor c) = show c

unClutterColor (ClutterColor o) = o

--withClutterColor (ClutterColor x) = withForeignPtr x

mkClutterColor :: Ptr ClutterColor -> IO ClutterColor
mkClutterColor colorPtr = do
  clutterForeignPtr <- newForeignPtr_ colorPtr
  return (ClutterColor clutterForeignPtr)

