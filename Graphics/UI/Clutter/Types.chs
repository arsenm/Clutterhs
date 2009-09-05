-- -*-haskell-*-
-- {-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}


module Graphics.UI.Clutter.Types (
--                                  module System.Glib.GObject,
                                  ClutterColor,
--                                  mkColor,
                                  unColor
                                 ) where

--import C2HS

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr, unsafeForeignPtrToPtr)
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


{#pointer *ClutterColor as ClutterColor foreign newtype #}

unColor (ClutterColor o) = o
--mkColor = ClutterColor

--class GObjectClass o => DrawableClass o
--toDrawable :: DrawableClass o => o -> Drawable
--toDrawable = unsafeCastGObject . toGObject

--instance GObjectClass ClutterColor where  --cluttercolor is not a gobject
--  toGObject = mkGObject . castForeignPtr . unColor
--  unsafeCastGObject = mkColor . castForeignPtr . unGObject


