{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Projection (project, unproject) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Camera

-- public functions for JS V3R
foreign import javascript unsafe "($2)['project']($1)"
    thr_project :: JSVal -> JSVal -> Three ()

project :: Camera -> V3R -> Three V3R
project c v = do
    jv <- mkTVector3 v
    thr_project (toJSVal c) (toJSVal jv)
    toVector3 jv

foreign import javascript unsafe "($2)['unproject']($1)"
    thr_unproject :: JSVal -> JSVal -> Three ()

unproject :: Camera -> V3R -> Three V3R
unproject c v = do
    jv <- mkTVector3 v
    thr_unproject (toJSVal c) (toJSVal jv)
    toVector3 jv
