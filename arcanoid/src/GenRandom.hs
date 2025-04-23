module GenRandom
    ( genRandomPosition
    , genRandomRotation
    , genRandomFloat
    ) where

import System.Random (StdGen, randomR)

import Vectors
import Types

-- [FUNCTIONS] --

genRandomPosition :: StdGen -> Position -> Position -> (Position, StdGen)
genRandomPosition gen 
    (Position (Vector2 l d)) 
    (Position (Vector2 r u)) = 
        let (x, gen') = randomR (l, r) gen
            (y, gen'') = randomR (d, u) gen'
        in (Position (Vector2 x y), gen'')

genRandomRotation :: StdGen -> (Rotation, StdGen)
genRandomRotation gen = 
    --let (r, gen') = randomR (0, 2 * pi) gen
    let (r, gen') = randomR (0, 360) gen
    in (Rotation r, gen')

genRandomFloat :: (Float, Float) -> StdGen -> (Float, StdGen)
genRandomFloat range = randomR range