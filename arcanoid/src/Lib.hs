module Lib
    (
        Vector2,
        Position,
        Rotation,
        Score,
        Player,
        Asteroid,
        Enemy,
        Bullet,
        Scene
    ) where

-- [DATA TYPES] --

data Vector2 = Vector2 {vector2x :: Float, vector2y :: Float}

newtype ID = ID {idInt :: Int}

newtype Position = Position {positionVector2 :: Vector2}

newtype Rotation = Rotation {rotationFloat :: Float}

newtype Velocity = Velocity {velocityVector2 :: Vector2}

newtype Health = Health {healthInt :: Int}

newtype Damage = Damage {damageInt :: Int}

newtype Size = Size {sizeVector2 :: Vector2}

newtype Time = Time {timeFloat :: Float}

newtype Score = Score {scoreInt :: Int}

data BoxCollider = BoxCollider
    {
        boxColliderSize :: Size,
        boxColliderPosition :: Position
    }

data Player = Player
    {
        playerID :: ID,
        playerPosition :: Position,
        playerBoxCollider :: BoxCollider,
        playerHealth :: Health,
        playerCooldown :: Time,
        playerInvulnerableTime :: Time
    }

data Asteroid = Asteroid
    {
        asteroidID :: ID,
        asteroidPosition :: Position,
        asteroidRotation :: Rotation,
        asteroidVelocity :: Velocity,
        asteroidHealth :: Health,
        asteroidBoxCollider :: BoxCollider
    }

data Enemy = Enemy
    {
        enemyID :: ID,
        enemyPosition :: Position,
        enemyVelocity :: Velocity,
        enemyHealth :: Health,
        enemyCooldown :: Time,
        enemyBoxCollider :: BoxCollider
    }

data Bullet = Bullet
    {
        bulletID :: ID,
        bulletPosition :: Position,
        bulletVelocity :: Velocity,
        bulletBoxCollider :: BoxCollider
    }

data Scene = Scene
    {
        time :: Time,
        scenePlayer :: Player,
        sceneAsteroids :: [Asteroid],
        sceneEnemies :: [Enemy],
        scenePlayerBullets :: [Bullet],
        sceneEnemyBullets :: [Bullet],
        sceneScore :: Score
    }

-- [CLASSES] --

class Collidable a where
    getCollider :: a -> BoxCollider

-- [INSTANCES] --

--Data.vector?..
instance Num Vector2 where
    (+) (Vector2 vx vy) (Vector2 ux uy) = Vector2 (vx + ux) (vy + uy)
    (-) (Vector2 vx vy) (Vector2 ux uy) = Vector2 (vx - ux) (vy - uy)
    (*) (Vector2 vx vy) (Vector2 ux uy) = Vector2 (vx * ux) (vy * uy)
    abs vector@(Vector2 vx vy) = let mag = vectorMag vector in Vector2 mag mag
    signum vector@(Vector2 vx vy) = let mag = vectorMag vector in Vector2 (vx / mag) (vy / mag)
    fromInteger x = Vector2 (fromInteger x) 0.0

instance Collidable BoxCollider where
    getCollider = id

instance Collidable Player where
    getCollider = playerBoxCollider

instance Collidable Asteroid where
    getCollider = asteroidBoxCollider

instance Collidable Enemy where
    getCollider = enemyBoxCollider

instance Collidable Bullet where
    getCollider = bulletBoxCollider

-- [CONSTANTS] --

--defaultVelocity = Velocity {velocityVector2 = Vector2 {vector2x = 0, vector2y = -1}}
--defaultBulletVelocityUp = Velocity $ Vector2 {vector2x = 0, vector2y = 5}

playerReloadTime :: Time
playerReloadTime = Time 1.0

playerInvulnerableTimeout :: Time
playerInvulnerableTimeout = Time 3.0

defaultPlayer :: Player
defaultPlayer = Player
    {
        playerID = ID 0,
        playerPosition = Position $ Vector2 {vector2x = 0, vector2y = 0},
        playerBoxCollider = BoxCollider {boxColliderSize = Size $ Vector2 {vector2x = 10, vector2y = 10}},
        playerHealth = Health 3,
        playerCooldown = Time 0.0,
        playerInvulnerableTime = Time 0.0
    }

asteroidDamage :: Damage
asteroidDamage = Damage 2

defaultAsteroidHealth :: Health
defaultAsteroidHealth = Health 10

enemyDamage :: Damage
enemyDamage = Damage 1

enemyReloadTime :: Time
enemyReloadTime = Time 3.0

bulletDamage :: Damage
bulletDamage = Damage 1

defaultBulletSpeed :: Float
defaultBulletSpeed = 5.0

-- [FUNCTIONS] --

vectorMag :: Vector2 -> Float
vectorMag (Vector2 vx vy) = sqrt $ vx^2 + vy^2

vectorNorm :: Vector2 -> Vector2
vectorNorm = signum

velocityFromRot :: Rotation -> Velocity
velocityFromRot (Rotation r) = Velocity $ Vector2 {vector2x = cos r,
                                                   vector2y = sin r}

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides x y | abs (vector2x x_col_pos_v2 - vector2x y_col_pos_v2) <
               vector2x x_col_size_v2 + vector2x y_col_size_v2 = True
             | abs (vector2y x_col_pos_v2 - vector2y y_col_pos_v2) <
               vector2y x_col_size_v2 + vector2y y_col_size_v2 = True
             | otherwise = False
    where 
        x_col = getCollider x
        y_col = getCollider y
        x_col_size_v2 = sizeVector2 $ boxColliderSize x_col
        y_col_size_v2 = sizeVector2 $ boxColliderSize y_col
        x_col_pos_v2 = positionVector2 $ boxColliderPosition x_col
        y_col_pos_v2 = positionVector2 $ boxColliderPosition y_col

damage :: Health -> Damage -> Health
damage (Health hp) (Damage dmg) = Health (hp - dmg)

--checkCollisions :: Player -> [Asteroid] -> [Enemy] -> [Bullet] -> [Bullet] -> [(ID, ID)]

--updatePlayer :: Time -> Player -> [Asteroid] -> [Enemy] -> [Bullet] -> [(ID, ID)] -> Player

--updateAsteroid :: Time -> Asteroid -> Damage -> Asteroid

--updateAsteroids :: Time -> [Asteroid] -> [Bullet] -> [(ID, ID)] -> [Asteroid]

--updateEnemy :: Time -> Enemy -> [Bullet] -> [(ID, ID)] -> Enemy

--updateEnemies :: Time -> [Enemy] -> [Bullet] -> [(ID, ID)] -> [Enemy]

--updatePlayerBullets :: Time -> [Bullet] -> [(ID, ID)] -> [Bullet]

--updateScene :: Time -> Scene -> Scene
