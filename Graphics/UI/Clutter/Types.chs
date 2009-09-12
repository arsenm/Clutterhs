-- -*-haskell-*-
-- {-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

#include <clutter/clutter.h>

{# context lib="clutter" prefix="clutter" #}


module Graphics.UI.Clutter.Types (
                                  Color,
                                  mkColor,
                                  unColor,
                                  colorRed,
                                  Actor,
                                  ActorClass,
                                  withActor,
                                  mkActor,
                                  unActor,
                                  toActor,

                                  Stage,
                                  StageClass,
                                  mkStage,
                                  unStage,

                                  makeNewObject,
                                  InitError(..)
                                 ) where

import C2HS
import System.Glib.GType (GType, typeInstanceIsA)
import System.Glib.GObject
import Control.Monad (when)


-- *************************************************************** Misc

{# enum ClutterInitError as InitError {underscoreToCase} deriving (Show, Eq) #}

-- *************************************************************** Misc

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

{# pointer *ClutterColor as Color foreign newtype #}

--what is this unsafe madness?
--this is just a play with c2hs
--colorRed (Color c) = {# get Color->alpha #} $ unsafeForeignPtrToPtr c
colorRed (Color c) = withForeignPtr c {# get Color->alpha #}

instance Show Color where
    show (Color c) = show c

unColor (Color o) = unsafeForeignPtrToPtr o

--withColor (Color o) = withForeignPtr o

{-
manageColor :: Color -> IO ()
manageColor (Color colorForeignPtr) = do
  addForeignPtrFinalizer colorDestroy colorForeignPtr

foreign import ccall unsafe "&clutter_color_free"
  colorDestroy :: FinalizerPtr Color
-}

mkColor :: Ptr Color -> IO Color
mkColor colorPtr = do
  clutterForeignPtr <- newForeignPtr_ colorPtr
  return (Color clutterForeignPtr)

{-
gtk skips this ginitiallyunowned stuff...seems useless anyway
{# pointer *GInitiallyUnowned as GInitiallyUnowned foreign newtype #}

--first attempt at class such stuff.

--class GInitiallyUnowned ???

mkGInitiallyUnowned = GInitiallyUnowned
unGInitiallyUnowned (GInitiallyUnowned o) = o
-}

-- *************************************************************** Actor

{# pointer *ClutterActor as Actor foreign newtype #}

mkActor = Actor
unActor (Actor a) = a

class GObjectClass o => ActorClass o
toActor::ActorClass o => o -> Actor
toActor = unsafeCastGObject . toGObject

instance ActorClass Actor
instance GObjectClass Actor where
  toGObject = mkGObject . castForeignPtr . unActor
  unsafeCastGObject = mkActor . castForeignPtr . unGObject

{- -- doesn't exist
castToActor :: GObjectClass obj => obj -> Actor
castToActor = castTo gTypeActor "Actor"

gTypeActor = {# call fun unsafe clutter_actor_get_type #}
-}

--class GObjectClass o => ObjectClass o
--toObject :: ObjectClass o => o -> Object
--toObject = unsafeCastGObject . toGObject

--class GInitiallyUnownedClass o => ActorClass o

-- ***************************************************************

-- *************************************************************** Group

{#pointer *ClutterGroup as Group foreign newtype #}

class ActorClass o => GroupClass o
toGroup :: GroupClass o => o -> Group
toGroup = unsafeCastGObject . toGObject

mkGroup = Group
unGroup (Group o) = o

instance GroupClass Group
instance ActorClass Group
instance GObjectClass Group where
  toGObject = mkGObject . castForeignPtr . unGroup
  unsafeCastGObject = mkGroup . castForeignPtr . unGObject

-- *************************************************************** Group

-- *************************************************************** Stage

{# pointer *ClutterStage as Stage foreign newtype #}

class GroupClass o => StageClass o
toStage :: StageClass o => o -> Stage
toStage = unsafeCastGObject . toGObject

mkStage = Stage
unStage (Stage o) = o

instance StageClass Stage
instance GroupClass Stage
instance ActorClass Stage
instance GObjectClass Stage where
  toGObject = mkGObject . castForeignPtr . unStage
  unsafeCastGObject = mkStage . castForeignPtr . unGObject

-- ***************************************************************

--taken / modified from gtk2hs...not sure why they have ObjectClass
--adn not GObject, also why do they use own newForeignPtr?....Figure it out later
makeNewObject :: GObjectClass obj =>
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewObject constr generator = do
  objPtr <- generator
  when (objPtr == nullPtr) (fail "makeNewObject: object is NULL")
  objectRefSink objPtr
  obj <- newForeignPtr objectUnref objPtr
  return $! constr obj

