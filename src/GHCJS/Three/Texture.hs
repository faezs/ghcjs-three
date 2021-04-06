{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Texture (Texture(..), mkTexture, setNeedsUpdate,
                            TextureLoader(..), mkTextureLoader, loadTexture,
    ImageLoader(..), mkImageLoader, setCrossOrigin, loadImage) where

import Data.Functor
import GHCJS.Foreign.Callback
import GHCJS.Concurrent

import GHCJS.DOM.Types        hiding (toJSVal, fromJSVal)

import GHCJS.Three.Monad
import GHCJS.Three.Disposable

newtype Texture = Texture {
    getTextureObject :: BaseObject
} deriving (ThreeJSVal)

instance Disposable Texture

foreign import javascript unsafe "new window['THREE']['Texture']($1)"
    thr_mkTexture :: JSVal -> Three JSVal

mkTexture :: HTMLCanvasElement -> Three Texture
mkTexture img = fromJSVal <$> thr_mkTexture (unGObject $ toGObject img)

foreign import javascript unsafe "($2)['needsUpdate'] = $1 === 1"
    thr_setNeedsUpdate :: Int -> JSVal -> Three ()

setNeedsUpdate :: Bool -> Texture -> Three ()
setNeedsUpdate u t = thr_setNeedsUpdate (if u then 1 else 0) $ toJSVal t

newtype TextureLoader = TextureLoader {
    getTextureLoaderObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window['THREE']['TextureLoader']()"
    thr_mkTextureLoader :: Three JSVal

mkTextureLoader :: Three TextureLoader
mkTextureLoader = fromJSVal <$> thr_mkTextureLoader

foreign import javascript interruptible "($2)['load']($1, $c);"
    thr_loadTexture :: JSString -> JSVal -> Three JSVal

loadTexture :: JSString -> TextureLoader -> Three Texture
loadTexture url loader = fromJSVal <$> thr_loadTexture url (toJSVal loader)



newtype ImageLoader = ImageLoader {
    getImageLoaderObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window['THREE']['ImageLoader']()"
    thr_mkImageLoader :: Three JSVal

mkImageLoader :: Three ImageLoader
mkImageLoader = fromJSVal <$> thr_mkImageLoader

foreign import javascript unsafe "($2)['setCrossOrigin']($1)"
    thr_setCrossOrigin :: JSString -> JSVal -> Three ()

setCrossOrigin :: JSString -> ImageLoader -> Three ()
setCrossOrigin url loader = thr_setCrossOrigin url $ toJSVal loader

foreign import javascript unsafe "($5)['load']($1, $2, $3, $4)"
    thr_load :: JSString -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> JSVal -> Three ()

loadImage :: JSString -> (BaseObject -> IO ()) -> (BaseObject -> IO ()) -> (BaseObject -> IO ()) -> ImageLoader -> Three ()
loadImage url onLoad onProgress onError loader = do
    loadCB <- syncCallback1 ContinueAsync (onLoad . fromJSVal)
    progCB <- syncCallback1 ContinueAsync (onProgress . fromJSVal)
    errCB  <- syncCallback1 ContinueAsync (onError . fromJSVal)
    thr_load url loadCB progCB errCB $ toJSVal loader
