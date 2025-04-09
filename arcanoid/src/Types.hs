module Types
    ( ID(ID)
    , Time(Time)
    , Health(Health)
    , Damage(Damage)
    , Rotation(Rotation)
    , Score(Score)
    , Position(Position)
    , Velocity(Velocity)
    , Size(Size)
    , BoxCollider (BoxCollider), boxColliderPosition, boxColliderSize
    , Player(Player), playerID, playerPosition, playerVelocity, playerBoxCollider, playerHealth, playerCooldown, playerInvulnerableTime
    , Asteroid(Asteroid), asteroidID, asteroidPosition, asteroidRotation, asteroidVelocity, asteroidHealth, asteroidBoxCollider
    , Enemy(Enemy), enemyID, enemyPosition, enemyVelocity, enemyHealth, enemyCooldown, enemyBoxCollider
    , Bullet(Bullet), bulletID, bulletPosition, bulletVelocity, bulletBoxCollider
    , CollisionList(CollisionList), collisionListPlayerAsteroids, collisionListPlayerBullet, collisionListPlayerEnemy, collisionListAsteroidBullet, collisionListEnemyBullet
    , Scene(Scene), sceneID, sceneTime, scenePlayer, sceneAsteroids, sceneEnemies, scenePlayerBullets, sceneEnemyBullets, sceneScore, sceneGen
    , Collidable, getCollider
    , Identifiable, getID
    , Killable, isAlive
    , Transformable, getPosition
    ) where

import System.Random (StdGen)

import Vectors

-- [DATA TYPES] --

newtype ID = ID Int
    deriving Eq

newtype Position = Position Vector2

newtype Rotation = Rotation Float

newtype Velocity = Velocity Vector2

newtype Health = Health Int

newtype Damage = Damage Int

newtype Size = Size Vector2

newtype Time = Time Float
    deriving (Eq, Ord)

newtype Score = Score Int

data BoxCollider = BoxCollider
    {
        boxColliderSize :: Size,
        boxColliderPosition :: Position
    }

data Player = Player
    {
        playerID :: ID,
        playerPosition :: Position,
        playerVelocity :: Velocity,
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
        sceneID :: ID,
        sceneTime :: Time,
        scenePlayer :: Player,
        sceneEnemies :: [Enemy],
        sceneAsteroids :: [Asteroid],
        scenePlayerBullets :: [Bullet],
        sceneEnemyBullets :: [Bullet],
        sceneScore :: Score,
        sceneGen :: StdGen
    }

data CollisionList = CollisionList
    {
        collisionListPlayerBullet :: [(ID, ID)],
        collisionListPlayerEnemy :: [(ID, ID)],
        collisionListPlayerAsteroids :: [(ID, ID)],
        collisionListAsteroidBullet :: [(ID, ID)],
        collisionListEnemyBullet :: [(ID, ID)]
    }

-- [CLASSES] --

class Collidable a where
    getCollider :: a -> BoxCollider

class Identifiable a where
    getID :: a -> ID

class Killable a where
    isAlive :: a -> Bool

class Transformable a where
    getPosition :: a -> Position

-- [INSTANCES] --

instance Collidable BoxCollider where
    getCollider = id

instance Collidable Player where
    getCollider = playerBoxCollider

instance Collidable Enemy where
    getCollider = enemyBoxCollider

instance Collidable Asteroid where
    getCollider = asteroidBoxCollider

instance Collidable Bullet where
    getCollider = bulletBoxCollider

instance Identifiable Player where
    getID = playerID

instance Identifiable Enemy where
    getID = enemyID

instance Identifiable Asteroid where
    getID = asteroidID

instance Identifiable Bullet where
    getID = bulletID

instance Transformable Player where
    getPosition = playerPosition

instance Transformable Enemy where
    getPosition = enemyPosition

instance Transformable Asteroid where
    getPosition = asteroidPosition

instance Transformable Bullet where
    getPosition = bulletPosition

instance Killable Asteroid where
    isAlive asteroid = health > 0
        where
            Health health = asteroidHealth asteroid

instance Killable Enemy where
    isAlive enemy = health > 0
        where
            Health health = enemyHealth enemy