-- -*-haskell-*-
-- {-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}


module Graphics.UI.Clutter.Types (
                                  Color,
                                  mkColor,
                                  unColor,
--                                  colorRed
                                 ) where

import C2HS
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

--colorRed (Color c) = {# get Color->red #} c


{# pointer *ClutterColor as Color foreign newtype #}

instance Show Color where
    show (Color c) = show c

unColor (Color o) = o

--withColor (Color x) = withForeignPtr x

mkColor :: Ptr Color -> IO Color
mkColor colorPtr = do
  clutterForeignPtr <- newForeignPtr_ colorPtr
  return (Color clutterForeignPtr)

