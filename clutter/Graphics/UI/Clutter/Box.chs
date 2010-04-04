-- -*-haskell-*-
--  Clutter Box
--
--  Author : Matthew Arsenault
--
--  Created: 27 Mar 2010
--
--  Copyright (C) 2010 Matthew Arsenault
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

module Graphics.UI.Clutter.Box (
#if CLUTTER_CHECK_VERSION(1,2,0)
-- * Description

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |    +----'Actor'
-- |           +----'Box'
-- @
--

-- * Types
  Box,
  BoxClass,

-- * Constructors
  boxNew,

-- * Methods
  boxSetLayoutManager,
  boxGetLayoutManager,
  boxSetColor,
  boxGetColor,
  boxPack,
--boxPackAfter,
--boxPackBefore,
--boxPackAt,

-- * Attributes
  boxColor,
  boxColorSet,
  boxLayoutManager
#endif
  ) where

#if CLUTTER_CHECK_VERSION(1,2,0)

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Utility #}
{# import Graphics.UI.Clutter.StoreValue #}
{# import Graphics.UI.Clutter.AnimOp #}

import System.Glib.Attributes
import System.Glib.Properties
import Control.Monad (liftM, foldM_)
import C2HS

-- | Creates a new 'Box'. The children of the box will be layed out by
-- the passed manager
--
-- [@manager@] a 'LayoutManager'
--
-- [@Returns@] the newly created 'Box' actor
--
-- * Since 1.2
--
boxNew :: (LayoutManagerClass manager) => manager -> IO (Box manager)
boxNew m = withLayoutManagerClass m $ \mPtr ->
  newBox m =<< {# call unsafe box_new #} mPtr


-- | Sets the 'LayoutManager' for box
--
-- A 'LayoutManager' is a delegate object that controls the layout of
-- the children of box
--
-- [@box@] a 'Box'
--
-- [@manager@] a 'LayoutManager'
--
-- * Since 1.2
--
{# fun unsafe box_set_layout_manager as ^
  `(LayoutManagerClass manager)' => { withBox* `Box manager', withLayoutManagerClass* `manager' } -> `()' #}



-- | Retrieves the 'LayoutManager' instance used by box
--
-- [@box@] a 'Box'
--
-- [@Returns@] a 'LayoutManager'
--
-- * Since 1.2
--
boxGetLayoutManager :: (LayoutManagerClass manager) => Box manager -> IO manager
boxGetLayoutManager box = withBox box $ \boxPtr ->
  liftM unsafeCastLayoutManager $ newLayoutManager =<< {# call unsafe box_get_layout_manager #} boxPtr


-- | Retrieves the background color of box
--
-- If the 'boxColorSet' property is set to @False@ the returned
-- 'Color' is @Nothing@
--
-- [@box@] a 'Box'
--
-- [@Returns@] Maybe a 'Color'
--
-- * Since 1.2
--
{# fun unsafe box_set_color as ^
  `(LayoutManagerClass manager)' => { withBox* `Box manager', withMaybeColor* `Maybe Color' } -> `()' #}



-- | Retrieves the background color of box
--
-- If the 'boxColorSet' property is set to @False@, @Nothing@ is
-- returned
--
-- [@box@] a 'Box'
--
-- [@Returns@] the set 'Color'
--
-- * Since 1.2
--
boxGetColor :: (LayoutManagerClass manager) => Box manager -> IO (Maybe Color)
boxGetColor b = withBox b $ \bPtr -> do
  cs <- get b boxColorSet
  if cs
     then alloca $ \cPtr -> do
            {# call unsafe box_get_color #} bPtr cPtr
            Just `fmap` peek cPtr
     else return Prelude.Nothing



-- | Adds actor to box and sets layout properties at the same time, if
-- the 'LayoutManager' used by box has them
--
-- This function is a wrapper around 'containerAddActor' and
-- 'layoutManagerChildSet'
--
-- [@box@] a 'Box'
--
-- [@actor@] an 'Actor'
--
-- [@properties@] List of attributes and values to set
--
-- * Since 1.2
--
boxPack :: (LayoutManagerClass a, ActorClass actor) => Box a -> actor -> [AnimOp a] -> IO ()
boxPack box actor us =
    let (names, gvals) = toPropertyLists us
        boxPackv = {# call unsafe box_packv #}
    in
    withMany withCString names $ \cstrs ->
      withArrayLen cstrs $ \len strptr ->
        withBox box $ \boxPtr ->
          withActorClass actor $ \actptr ->
            withArray gvals $ \gvPtr -> do
              boxPackv boxPtr actptr (cIntConv len) strptr gvPtr
              foldM_ unsetOneGVal gvPtr gvals


-- No box_pack_before/afterv?



-- Attributes

-- | The color to be used to paint the background of the
-- 'Box'. Setting this property will set the 'boxColorSet' property as
-- a side effect
--
-- * Since 1.2
--
boxColor :: LayoutManagerClass a => Attr (Box a) (Maybe Color)
boxColor = newNamedAttr "color" boxGetColor boxSetColor


-- | Whether the 'boxColor' property has been set
--
-- Default value: @False@
--
-- * Since 1.2
--
boxColorSet :: LayoutManagerClass a => ReadAttr (Box a) Bool
boxColorSet = readAttrFromBoolProperty "color-set"


-- | The 'LayoutManager' used by the 'Box'
--
-- * Since 1.2
--
boxLayoutManager :: LayoutManagerClass a => Attr (Box a) a
boxLayoutManager = newNamedAttr "layout-manager" boxGetLayoutManager boxSetLayoutManager

#endif

