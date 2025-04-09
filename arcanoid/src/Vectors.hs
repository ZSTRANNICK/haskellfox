module Vectors
    ( Vector2(..)
    , limitVector2
    , vector2FromPolar
    ) where

import Utils

-- [DATA TYPES] --

data Vector2 = Vector2 {vector2x :: Float, vector2y :: Float}
    deriving Eq

-- [INSTANCES] --

instance Num Vector2 where
    (+) (Vector2 vx vy) (Vector2 ux uy) = Vector2 (vx + ux) (vy + uy)
    (-) (Vector2 vx vy) (Vector2 ux uy) = Vector2 (vx - ux) (vy - uy)
    (*) (Vector2 vx vy) (Vector2 ux uy) = Vector2 (vx * ux) (vy * uy)
    abs vector = let mag = vectorMag vector in Vector2 mag mag
    signum vector@(Vector2 vx vy) = let mag = vectorMag vector in Vector2 (vx / mag) (vy / mag)
    fromInteger x = Vector2 (fromInteger x) 0.0

-- [FUNCTIONS] --

vectorMag :: Vector2 -> Float
vectorMag (Vector2 vx vy) = sqrt $ vx * vx + vy * vy

-- vectorNorm :: Vector2 -> Vector2
-- vectorNorm = signum

vector2FromPolar :: Float -> Float -> Vector2
vector2FromPolar len rot = Vector2
    { vector2x = len * (cos rot)
    , vector2y = len * (sin rot)}

limitVector2 :: Vector2 -> Vector2 -> Vector2 -> Vector2
limitVector2 ld ru v = Vector2 x y
    where
        x = limitOrd (vector2x ld) (vector2x ru) (vector2x v)
        y = limitOrd (vector2y ld) (vector2y ru) (vector2y v)
