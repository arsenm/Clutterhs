-- -*-haskell-*-
--  Clutter Texture
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

module Graphics.UI.Clutter.Texture (
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'GInitiallyUnowned'
-- |         +----'Actor'
-- |               +----'Texture'
-- |                     +----'CairoTexture'
-- @

-- * Constructors
  textureNew,
  textureNewFromFile,
  textureNewFromActor,

-- * Methods

  textureSetFromFile,
  textureSetFromRgbData,  --TODO: RGB??? vs. Rgb
--textureSetFromYuvData,
--textureSetAreaFromRgbData,
  textureGetBaseSize,

--textureGetPixelFormat,
  textureGetMaxTileWaste,

  textureGetFilterQuality,
  textureSetFilterQuality,

--textureGetCoglTexture,
--textureSetCoglTexture,

--textureSetCoglMaterial,
--textureGetCoglMaterial,

  textureGetSyncSize,
  textureSetSyncSize,

  textureGetRepeat,
  textureSetRepeat,

  textureSetKeepAspectRatio,
  textureGetKeepAspectRatio,

  textureGetLoadAsync,
  textureSetLoadAsync,

  textureGetLoadDataAsync,
  textureSetLoadDataAsync,

-- * Attributes
  textureBaseSize,
  textureFilterQuality,
--textureCoglTexture,
--textureCoglMaterial,
  textureSyncSize,
  textureRepeat,
  textureKeepAspectRatio,
  textureLoadAsync,
  textureLoadDataAsync,

-- * Signals
  onLoadFinished,
  afterLoadFinished,
  loadFinished,

  onPixbufChange,
  afterPixbufChange,
  pixbufChange,

  onSizeChange,
  afterSizeChange,
  sizeChange
  ) where

{# import Graphics.UI.Clutter.Types #}
{# import Graphics.UI.Clutter.Signals #}
{# import Graphics.UI.Clutter.Utility #}

import Data.Array.Base (getBounds)

import C2HS
import Control.Monad (liftM)
import System.Glib.Attributes
import System.Glib.GError


-- | Creates a new empty 'Texture' object.
--
-- [@Returns@] A newly created 'Texture' object.
--
{# fun unsafe texture_new as ^ { } -> `Texture' newTexture* #}


-- | Creates a new 'Texture' actor to display the image contained a
--   file. If the image failed to load then a GError exception is
--   thrown.
--
-- [@filename@] The name of an image file to load.
--
-- [@Returns@] A newly created 'Texture'
--
-- * Since 0.8
--
textureNewFromFile :: String -> IO Texture
textureNewFromFile filename = let func = {# call unsafe texture_new_from_file #}
                              in newTexture =<< (propagateGError $ \gerrorPtr ->
                                   withCString filename $ \cstr -> do
                                     func cstr gerrorPtr)



-- | Creates a new 'Texture' object with its source a prexisting actor
--   (and associated children). The textures content will contain
--   'live' redirected output of the actors scene.
--
-- Note this function is intented as a utility call for uniformly
-- applying shaders to groups and other potential visual effects. It
-- requires that the CLUTTER_FEATURE_OFFSCREEN feature is supported by
-- the current backend and the target system.
--
-- Some tips on usage:
--
-- * The source actor must be made visible (i.e by calling 'actorShow').
--
-- * The source actor must have a parent in order for it to be
--   allocated a size from the layouting mechanism. If the source
--   actor does not have a parent when this function is called then
--   the 'Texture' will adopt it and allocate it at its preferred
--   size. Using this you can clone an actor that is otherwise not
--   displayed. Because of this feature if you do intend to display
--   the source actor then you must make sure that the actor is
--   parented before calling 'textureNewFromActor' or that you
--   unparent it before adding it to a container.
--
-- * When getting the image for the clone texture, Clutter will
--   attempt to render the source actor exactly as it would appear if
--   it was rendered on screen. The source actor's parent
--   transformations are taken into account. Therefore if your source
--   actor is rotated along the X or Y axes so that it has some depth,
--   the texture will appear differently depending on the on-screen
--   location of the source actor. While painting the source actor,
--   Clutter will set up a temporary asymmetric perspective matrix as
--   the projection matrix so that the source actor will be projected
--   as if a small section of the screen was being viewed. Before
--   version 0.8.2, an orthogonal identity projection was used which
--   meant that the source actor would be clipped if any part of it
--   was not on the zero Z-plane.
--
-- * Avoid reparenting the source with the created texture.
--
-- * A group can be padded with a transparent rectangle as to provide
--   a border to contents for shader output (blurring text for
--   example).
--
--  The texture will automatically resize to contain a further
--  transformed source. However, this involves overhead and can be
--  avoided by placing the source actor in a bounding group sized
--  large enough to contain any child tranformations.
--
-- * Uploading pixel data to the texture (e.g by using
--   'textureSetFromFile') will destroy the offscreen texture data and
--   end redirection.
--
-- * cogl_texture_get_data() with the handle returned by
--   'textureGetCoglTexture' can be used to read the offscreen texture
--   pixels into a pixbuf.
--
-- [@actor@] A source Actor
--
-- [@Returns@] @Just@ A newly created 'Texture' object, or @Nothing@ on failure.
--
-- * Since 0.6
--
{# fun unsafe texture_new_from_actor as ^
       `(ActorClass a)' => { withActorClass* `a'} -> `Maybe Texture' maybeNewTexture* #}


--FIXME: Better description of gerror exception
-- | Sets the ClutterTexture image data from an image file. In case of
--   failure, @False@ is returned and a GError exception is thrown.
--
-- If "load-async" is set to @True@, this function will return as soon
-- as possible, and the actual image loading from disk will be
-- performed asynchronously. "size-change" will be emitten when the
-- size of the texture is available and "load-finished" will be
-- emitted when the image has been loaded or if an error occurred.
--
-- [@texture@] A 'Texture'
--
-- [@filename@] The filename of the image in GLib file name encoding
--
-- [@Returns@] @True@ if the image was successfully loaded and set
--
-- * Since 0.8
--
textureSetFromFile :: Texture -> String -> IO Bool
textureSetFromFile txt fname = let func = {# call unsafe texture_set_from_file #}
                               in liftM cToBool $ propagateGError $ \gerrorPtr ->
                                    withTexture txt $ \txtptr ->
                                      withCString fname $ \cstr ->
                                        func txtptr cstr (castPtr gerrorPtr)




--CHECKME: Generalized RGBData
--CHECKME: Rgb or RGB?
--FIXME: Proper rowstride
textureSetFromRgbData :: Texture ->
                         RGBData Int Word8 ->
                         [TextureFlags] ->
                         IO Bool
textureSetFromRgbData txt dat flags =
    let func = {# call unsafe texture_set_from_rgb_data #}
        hasA = rgbDataHasAlpha dat
        bpp = if hasA then 4 else 3
        datPtr = rgbDataData dat
    in withTexture txt $ \txtptr ->
        propagateGError $ \gerrorPtr -> do
          (w, h) <- getBounds dat
          putStrLn $ "W: " ++ Prelude.show w ++ " H: " ++ Prelude.show h
          let rowstride = w * 4 -- FIXME
          liftM cToBool $ withForeignPtr datPtr $ \fPtr ->
            func txtptr
                 (castPtr fPtr)
                 (cFromBool hasA)
                 (cIntConv w)
                 (cIntConv h)
                 (cIntConv rowstride)
                 (cIntConv bpp)
                 (cFromFlags flags)
                 (castPtr gerrorPtr)

{-
texture_set_from_yuv_data
texture_set_area_from_rgb_data
-}


-- | Gets the size in pixels of the untransformed underlying image
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] (width, height)
--
{# fun unsafe texture_get_base_size as ^
       { withTexture* `Texture',
         alloca- `Int' peekIntConv*,
         alloca- `Int' peekIntConv* } -> `()' #}

textureBaseSize :: ReadAttr Texture (Int, Int)
textureBaseSize = readAttr textureGetBaseSize

{- TODO: Requires cogl
{# fun unsafe texture_get_pixel_format as ^
       { withTexture* `Texture' } -> `CoglPixelFormat' cToEnum #}
-}

--CHECKME: Wrap the -1 thing somehow?
-- | Gets the maximum waste that will be used when creating a texture
--   or -1 if slicing is disabled.
--
-- [@texture@] A 'Texture'
--
-- [@Returns@] The maximum waste or -1 if the texture waste is
-- unlimited.
--
-- * Since 0.8
--
{# fun unsafe texture_get_max_tile_waste as ^
       { withTexture* `Texture' } -> `Int' #}

-- | Gets the filter quality used when scaling a texture.
--
-- [@texture@] A 'Texture
--
-- [@Returns @] The filter quality value.
--
-- * Since 0.8
--
{# fun unsafe texture_get_filter_quality as ^
       { withTexture* `Texture' } -> `TextureQuality' cToEnum #}

-- | Sets the filter quality when scaling a texture. The quality is an
--  enumeration currently the following values are supported:
--  'TextureQualityLow' which is fast but only uses nearest neighbour
--  interpolation. 'TextureQualityMedium' which is computationally a
--  bit more expensive (bilinear interpolation), and
--  'TextureQualityHigh' which uses extra texture memory resources to
--  improve scaled down rendering as well (by using mipmaps). The
--  default value is 'TextureQualityMedium'.
--
-- [@texture@] a 'Texture'
--
-- [@filter_quality@] new filter quality value
--
-- * Since 0.8
--
{# fun unsafe texture_set_filter_quality as ^
       { withTexture* `Texture', cFromEnum `TextureQuality' } -> `()' #}

textureFilterQuality :: Attr Texture TextureQuality
textureFilterQuality = newAttr textureGetFilterQuality textureSetFilterQuality

{-
{# fun unsafe texture_get_cogl_texture as ^
       { withTexture* `Texture' } -> `CoglTexture' newCoglTexture #}
{# fun unsafe texture_set_cogl_texture as ^
       { withTexture* `Texture', withCoglTexture `CoglTexture' } -> `()' #}
textureCoglTexture :: Attr Texture CoglTexture
textureCoglTexture = newAttr textureGetCoglTexture textureSetCoglTexture

{# fun unsafe texture_get_cogl_material as ^
       { withTexture* `Texture' } -> `CoglMaterial' newCoglMaterial #}
{# fun unsafe texture_set_cogl_material as ^
       { withTexture* `Texture', withCoglMaterial `CoglMaterial' } -> `()' #}
textureCoglMaterial :: Attr Texture CoglMaterial
textureCoglMaterial = newAttr textureGetCoglMaterial textureSetCoglMaterial
-}


-- | Retrieves the value set with 'textureGetSyncSize'
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] @True@ if the 'Texture' should have the same preferred
-- size of the underlying image data
--
-- * Since 1.0
--
{# fun unsafe texture_get_sync_size as ^ { withTexture* `Texture' } -> `Bool' #}

-- | Sets whether texture should have the same preferred size as the
--   underlying image data.
--
-- [@texture@] a 'Texture'
--
-- [@sync_size@] @True@ if the texture should have the same size of
-- the underlying image data
--
-- * Since 1.0
--
{# fun unsafe texture_set_sync_size as ^ { withTexture* `Texture', `Bool' } -> `()' #}

textureSyncSize :: Attr Texture Bool
textureSyncSize = newAttr textureGetSyncSize textureSetSyncSize


-- | Retrieves the horizontal and vertical repeat values set using
--   'textureSetRepeat'
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] (horizontal (x) repeat, vertical (y) repeat)
--
-- * Since 1.0
--
{# fun unsafe texture_get_repeat as ^
       { withTexture* `Texture', alloca- `Bool' peekBool*, alloca- `Bool' peekBool* } -> `()' #}

-- | Sets whether the texture should repeat horizontally or vertically
--   when the actor size is bigger than the image size
--
-- [@texture@] a 'Texture'
--
-- [@repeat_x@] @True@ if the texture should repeat horizontally
--
-- [@repeat_y@] @True@ if the texture should repeat vertically
--
-- * Since 1.0
--
{# fun unsafe texture_set_repeat as ^ { withTexture* `Texture', `Bool', `Bool' } -> `()' #}

textureRepeat :: Attr Texture (Bool, Bool)
textureRepeat = newAttr textureGetRepeat (tup2ToF textureSetRepeat)

-- | Retrieves the value set using 'textureGetKeepAspectRatio'
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] @True@ if the 'Texture' should maintain the aspect
-- ratio of the underlying image
--
-- * Since 1.0
--
{# fun unsafe texture_get_keep_aspect_ratio as ^ { withTexture* `Texture' } -> `Bool' #}

-- | Sets whether texture should have a preferred size maintaining the
--   aspect ratio of the underlying image
--
-- [@texture@] a 'Texture'
--
-- [@keep_aspect@] @True@ to maintain aspect ratio
--
-- * Since 1.0
--
{# fun unsafe texture_set_keep_aspect_ratio as ^ { withTexture* `Texture', `Bool'} -> `()' #}

textureKeepAspectRatio :: Attr Texture Bool
textureKeepAspectRatio = newAttr textureGetKeepAspectRatio textureSetKeepAspectRatio


-- | Retrieves the value set using 'textureSetLoadAsync'
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] @True@ if the 'Texture' should load the data from disk
-- asynchronously
--
-- * Since 1.0
--
{# fun unsafe texture_get_load_async as ^ { withTexture* `Texture' } -> `Bool' #}

--TODO: load-async link in doc
-- | Sets whether texture should use a worker thread to load the data
--   from disk asynchronously. Setting load_async to @True@ will make
--   'textureSetFromFile' return immediately.
--
-- See the "load-async" property documentation, and 'textureSetLoadDataAsync'.
--
-- [@texture@] a 'Texture'
--
-- [@load_async@] @True@ if the texture should asynchronously load
-- data from a filename
--
-- * Since 1.0
--
{# fun unsafe texture_set_load_async as ^ { withTexture* `Texture', `Bool'} -> `()' #}

textureLoadAsync :: Attr Texture Bool
textureLoadAsync = newAttr textureGetLoadAsync textureSetLoadAsync


-- | Retrieves the value set by 'textureSetLoadDataAsync'
--
-- [@texture@] a 'Texture'
--
-- [@Returns@] @True@ if the 'Texture' should load the image data from
-- a file asynchronously
--
-- * Since 1.0
--
{# fun unsafe texture_get_load_data_async as ^ { withTexture* `Texture' } -> `Bool' #}

-- | Sets whether texture should use a worker thread to load the data
--   from disk asynchronously. Setting load_async to @True@ will make
--   'textureSetFromFile' block until the 'Texture' has determined the
--   width and height of the image data.
--
-- See the "load-async" property documentation, and
-- 'textureSetLoadAsync'.
--
-- [@texture@] a 'Texture'
--
-- [@load_async@] @True@ if the texture should asynchronously load
-- data from a filename
--
-- * Since 1.0
--
{# fun unsafe texture_set_load_data_async as ^ { withTexture* `Texture', `Bool'} -> `()' #}

textureLoadDataAsync :: Attr Texture Bool
textureLoadDataAsync = newAttr textureGetLoadDataAsync textureSetLoadDataAsync

--FIXME: Make maybe error
--CHECKME: Does this GError stuff work?
onLoadFinished, afterLoadFinished :: Texture -> (GError -> IO ()) -> IO (ConnectId Texture)
onLoadFinished = connect_BOXED__NONE "load-finished" peek False
afterLoadFinished = connect_BOXED__NONE "load-finished" peek True

loadFinished :: Signal Texture (GError -> IO ())
loadFinished = Signal (connect_BOXED__NONE "load-finished" peek)


onPixbufChange, afterPixbufChange :: Texture -> IO () -> IO (ConnectId Texture)
onPixbufChange = connect_NONE__NONE "pixbuf-change" False
afterPixbufChange = connect_NONE__NONE "pixbuf-change" True

-- | The ::pixbuf-change signal is emitted each time the pixbuf used
--   by texture changes.
--
-- [@texture@] the texture which received the signal
--
pixbufChange :: Signal Texture (IO ())
pixbufChange = Signal (connect_NONE__NONE "pixbuf-change")


onSizeChange, afterSizeChange :: Texture -> (Int -> Int -> IO ()) -> IO (ConnectId Texture)
onSizeChange = connect_INT_INT__NONE "size-change" False
afterSizeChange = connect_INT_INT__NONE "size-change" True


-- | The ::size-change signal is emitted each time the size of the
-- pixbuf used by texture changes. The new size is given as argument
-- to the callback.
--
-- [@texture@] the texture which received the signal
--
-- [@width@] the width of the new texture
--
-- [@height@] the height of the new texture
--
sizeChange :: Signal Texture (Int -> Int -> IO ())
sizeChange = Signal (connect_INT_INT__NONE "size-change")

