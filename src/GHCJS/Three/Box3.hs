{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Box3 where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector

newtype Box3 = Box3 {
    box3Object :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "($1)['min']"
    thr_boxMin :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['max']"
    thr_boxMax :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['getCenter']()"
    thr_boxCenter :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['getSize']()"
    thr_boxSize :: JSVal -> Three JSVal

boxMin :: Box3 -> Three V3R
boxMin b = thr_boxMin (toJSVal b) >>= toVector3 . fromJSVal

boxMax :: Box3 -> Three V3R
boxMax b = thr_boxMax (toJSVal b) >>= toVector3 . fromJSVal

boxCenter :: Box3 -> Three V3R
boxCenter b = thr_boxCenter (toJSVal b) >>= toVector3 . fromJSVal

boxSize :: Box3 -> Three V3R
boxSize b = thr_boxSize (toJSVal b) >>= toVector3 . fromJSVal
