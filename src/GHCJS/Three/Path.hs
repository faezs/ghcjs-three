{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Path
    (Path(..), mkPath, IsPath(..)
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import GHCJS.Three.Monad
import GHCJS.Three.Vector

newtype Path = Path {
    pathObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window['THREE']['Path']($1)"
    thr_mkPath :: JSVal -> Three JSVal

mkPath :: [Vector2] -> Three Path
mkPath points = mapM mkTVector2 points >>= Marshal.toJSVal . map toJSVal >>= fmap fromJSVal . thr_mkPath

foreign import javascript unsafe "($2)['fromPoints']($1)"
    thr_fromPoints :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($3)['moveTo']($1, $2)"
    thr_moveTo :: Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($3)['lineTo']($1, $2)"
    thr_lineTo :: Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($5)['quadraticCurveTo']($1, $2, $3, $4)"
    thr_quadraticCurveTo :: Double -> Double -> Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($7)['bezierCurveTo']($1, $2, $3, $4, $5, $6)"
    thr_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> JSVal -> Three ()

class ThreeJSVal p => IsPath p where
    -- | Adds to the Path from the points. The first vector defines the offset. After that the lines get defined.
    pathFromPoints :: [V2R] -> p -> Three ()
    pathFromPoints points path = mapM mkTVector2 points >>= Marshal.toJSVal . map toJSVal >>= flip thr_fromPoints (toJSVal path)

    -- | This moves the offset to x and y
    pathMoveTo :: V2R -> p -> Three ()
    pathMoveTo (V2 x y) p = thr_moveTo x y (toJSVal p)

    -- | This creates a line from the offset to X and Y and updates the offset to X and Y.
    pathLineTo :: V2R -> p -> Three ()
    pathLineTo (V2 x y) p = thr_lineTo x y (toJSVal p)

    -- | This creates a quadratic curve from the offset to x and y with cpX and cpY as control point and updates the offset to x and y.
    quadraticCurveTo :: V2R -> V2R -> p -> Three ()
    quadraticCurveTo (V2 x y) (V2 x' y') p = thr_quadraticCurveTo x y x' y' (toJSVal p)

    -- | This creates a bezier curve from the last offset to x and y with cp1X, cp1Y and cp1X, cp1Y as control points and updates the offset to x and y.
    bezierCurveTo :: V2R -> V2R -> V2R -> p -> Three ()
    bezierCurveTo (V2 x1 y1) (V2 x2 y2) (V2 x y) p = thr_bezierCurveTo x1 y1 x2 y2 x y (toJSVal p)

instance IsPath Path
