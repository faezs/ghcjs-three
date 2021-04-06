{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Vector (
    IsJSVector(..), TVector3(..), TVector2(..), NormalVector,
    mkTVector3, toVector3, mkTVector2, toVector2, vector3To2, (#+), (#-),
    module Linear.V2,
    module Linear.V3
    ) where

import Data.Functor
import GHCJS.Types

import JavaScript.Array

import GHCJS.Three.Monad
import GHCJS.Three.HasXYZ
import GHCJS.Three.Matrix
import GHCJS.Three.CanCopy

import Linear.V2
import Linear.V3

foreign import javascript unsafe "($2)['add']($1)"
    thr_add :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['addScalar']($1)"
    thr_addScalar :: Double -> JSVal -> Three ()

foreign import javascript unsafe "($3)['addVectors']($1, $2)"
    thr_addVectors :: JSVal -> JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['angleTo']($1)"
    thr_angleTo :: JSVal -> JSVal -> Double

foreign import javascript unsafe "($2)['cross']($1)"
    thr_cross :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($3)['crossVectors']($1, $2)"
    thr_crossVectors :: JSVal -> JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['distanceTo']($1)"
    thr_distanceTo :: JSVal -> JSVal -> Double

foreign import javascript unsafe "($2)['distanceToSquared']($1)"
    thr_distanceToSquared :: JSVal -> JSVal -> Double

foreign import javascript unsafe "($2)['dot']($1)"
    thr_dot :: JSVal -> JSVal -> Double

foreign import javascript unsafe "($3)['fromArray']($1, $2)"
    thr_fromArray :: JSVal -> Int -> JSVal -> Three ()

foreign import javascript unsafe "($1)['length']()"
    thr_length :: JSVal -> Double

foreign import javascript unsafe "($1)['normalize']()"
    thr_normalize :: JSVal -> Three ()

foreign import javascript unsafe "($2)['multiplyScalar']($1)"
    thr_multiplyScalar :: Double -> JSVal -> Three ()


class ThreeJSVal v => IsJSVector v where
    addTo :: v -> v -> Three ()
    addTo v1 v2 = thr_add (toJSVal v1) (toJSVal v2)

    addScalar :: Double -> v -> Three ()
    addScalar s v = thr_addScalar s (toJSVal v)

    addVectors :: v -> v -> v -> Three ()
    addVectors v1 v2 v = thr_addVectors (toJSVal v1) (toJSVal v2) (toJSVal v)

    angleTo :: v -> v -> Double
    angleTo v1 v2 = thr_angleTo (toJSVal v1) (toJSVal v2)

    cross :: v -> v -> Three ()
    cross v1 v = thr_cross (toJSVal v1) (toJSVal v)

    crossVectors :: v -> v -> v -> Three ()
    crossVectors v1 v2 v = thr_crossVectors (toJSVal v1) (toJSVal v2) (toJSVal v)

    distanceTo :: v -> v -> Double
    distanceTo v1 v2 = thr_distanceTo (toJSVal v1) (toJSVal v2)

    distanceToSquared :: v -> v -> Double
    distanceToSquared v1 v2 = thr_distanceToSquared (toJSVal v1) (toJSVal v2)

    dot :: v -> v -> Double
    dot v1 v2 = thr_dot (toJSVal v1) (toJSVal v2)

    fromArray :: JSArray -> Int -> v -> Three ()
    fromArray arr idx v = thr_fromArray (jsval arr) idx (toJSVal v)

    vlength :: v -> Double
    vlength = thr_length . toJSVal

    normalize :: v -> Three ()
    normalize = thr_normalize . toJSVal

    multiplyScalar :: Double -> v -> Three ()
    multiplyScalar s v = thr_multiplyScalar s (toJSVal v)


-- JS version of 3D Vector
newtype TVector3 = TVector3 {
    vectorObject :: BaseObject
} deriving (ThreeJSVal)

instance IsJSVector TVector3
instance HasX TVector3
instance HasY TVector3
instance HasZ TVector3
instance CanApplyMatrix4 TVector3
instance CanCopy TVector3

foreign import javascript unsafe "($2)['setFromMatrixPosition']($1)"
    thr_setFromMatrixPosition :: JSVal -> JSVal -> Three ()

fromMatrixPosition :: Matrix4 -> Three V3
fromMatrixPosition m = do
    jv <- thr_mkVector3 0 0 0
    thr_setFromMatrixPosition (toJSVal m) jv
    toVector3 $ fromJSVal jv

-- JS version of 2D vector
newtype TVector2 = TVector2 {
    vector2Object :: BaseObject
} deriving (ThreeJSVal)

instance IsJSVector TVector2
instance HasX TVector2
instance HasY TVector2
instance CanCopy TVector2

-- normal vector is a special type of vector
type NormalVector = TVector3

foreign import javascript unsafe "new window['THREE']['Vector3']($1, $2, $3)"
    thr_mkVector3 :: Double -> Double -> Double -> Three JSVal

foreign import javascript unsafe "new window['THREE']['Vector2']($1, $2)"
    thr_mkVector2 :: Double -> Double -> Three JSVal

foreign import javascript unsafe "($1)['x']"
    thr_vecX :: JSVal -> Three Double
foreign import javascript unsafe "($1)['y']"
    thr_vecY :: JSVal -> Three Double
foreign import javascript unsafe "($1)['z']"
    thr_vecZ :: JSVal -> Three Double

vecX :: IsJSVector v => v -> Three Double
vecX = thr_vecX . toJSVal

vecY :: IsJSVector v => v -> Three Double
vecY = thr_vecY . toJSVal

vecZ :: IsJSVector v => v -> Three Double
vecZ = thr_vecZ . toJSVal

(#+) :: V3 -> V3 -> V3
(V3 x1 y1 z1) #+ (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

(#-) :: V3 -> V3 -> V3
(V3 x1 y1 z1) #- (V3 x2 y2 z2) = V3 (x1 - x2) (y1 - y2) (z1 - z2)

-- | create a new Three V3 object with TVector
mkTVector3 :: V3 -> Three TVector3
mkTVector3 (V3 x y z) = fromJSVal <$> thr_mkV3 x y z

mkTVector2 :: V2 -> Three TVector2
mkTVector2 (V2 x y) = fromJSVal <$> thr_mkVector2 x y

-- | convert Vector to TVector
toV3 :: TVector3 -> Three V3
toV3 v = V3 <$> vecX v <*> vecY v <*> vecZ v

toVector2 :: TVector2 -> Three V2
toVector2 v = V2 <$> vecX v <*> vecY v

-- | extract x and y fields of a 3D vector to form a new 2D vector
vector3To2 :: V3 -> V2
vector3To2 (V3 x y _) = V2 x y
