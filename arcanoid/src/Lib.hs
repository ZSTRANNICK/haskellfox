module Lib
    ( sceneChangePlayerVelocity 
    , sceneInstantiatePlayerBullet
    , updateScene
    ) where

import Utils
import Vectors
import Types
import GenRandom
import Constants

-- [FUNCTIONS] --

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides w z | abs (vector2x w_col_pos_v2 - vector2x z_col_pos_v2) <
               vector2x w_col_size_v2 + vector2x z_col_size_v2 && 
               abs (vector2y w_col_pos_v2 - vector2y z_col_pos_v2) <
               vector2y w_col_size_v2 + vector2y z_col_size_v2 = True
             | otherwise = False
    where 
        (BoxCollider w_col_size w_col_pos) = getCollider w
        (BoxCollider z_col_size z_col_pos) = getCollider z
        (Size w_col_size_v2) = w_col_size
        (Position w_col_pos_v2) = w_col_pos
        (Size z_col_size_v2) = z_col_size
        (Position z_col_pos_v2) = z_col_pos

checkCollisions :: Player -> [Asteroid] -> [Enemy] -> [Bullet] -> [Bullet] -> CollisionList
checkCollisions player asteroidList enemyList playerBulletList enemyBulletList = CollisionList {
    collisionListPlayerBullet = idsCollides [player] enemyBulletList,
    collisionListPlayerEnemy = idsCollides [player] enemyList,
    collisionListPlayerAsteroids = idsCollides [player] asteroidList,
    collisionListAsteroidBullet = idsCollides asteroidList playerBulletList,
    collisionListEnemyBullet = idsCollides enemyList playerBulletList
}

idsCollides :: (Collidable a, Collidable b, Identifiable a, Identifiable b) => [a] -> [b] -> [(ID, ID)]
idsCollides xs ys = [(getID x, getID y) | x <- xs, y <- ys, collides x y]

activateOnTime :: (Time -> Int) -> Time -> Time -> Bool
activateOnTime f previous current | (f current) - (f previous) > 0 = True
                                  | otherwise = False

powerIntervals :: Time -> Float -> Float -> Float -> Time -> Int
powerIntervals (Time interval) power offset mult (Time current) =
    round $ ((current + offset) / mult) ** power / interval * mult

instantiate :: (Identifiable a) => (ID -> Position -> a) -> ID ->  [Position] -> (ID, [a])
instantiate _ idnt [] = (idnt, [])
instantiate f idnt@(ID idnt_i) (x:xs) = (lastId, (f idnt x) : ys)
    where (lastId, ys) = instantiate f (ID $ idnt_i + 1) xs

instantiateFromTransformable :: (Transformable a, Identifiable b) => (ID -> Position -> b) -> ID -> Position -> [a] -> (ID, [b])
instantiateFromTransformable f idnt (Position offset) s = instantiate f idnt $ map ((\(Position x) -> Position $ x + offset) . getPosition) s

instantiateBullet :: Velocity -> ID -> Position -> Bullet
instantiateBullet velocity idnt position = Bullet {
            bulletID = idnt,
            bulletPosition = position,
            bulletVelocity = velocity,
            bulletBoxCollider = BoxCollider {
                boxColliderPosition = position,
                boxColliderSize = bulletSize
            }
        }

instantiateEnemy :: ID -> Position -> Enemy
instantiateEnemy idnt position = Enemy {
        enemyID = idnt,
        enemyPosition = position,
        enemyVelocity = defaultVelocity,
        enemyHealth = defaultEnemyHealth,
        enemyCooldown = enemyReloadTime,
        enemyBoxCollider = BoxCollider {
            boxColliderPosition = position,
            boxColliderSize = enemySize
        }
}

instantiateAsteroid :: Health -> Size -> Rotation -> ID -> Position -> Asteroid
instantiateAsteroid health size rotation idnt position = Asteroid {
        asteroidID = idnt,
        asteroidPosition = position,
        asteroidRotation = rotation,
        asteroidVelocity = defaultVelocity,
        asteroidHealth = health,
        asteroidBoxCollider = BoxCollider {
            boxColliderPosition = position,
            boxColliderSize = size
        }
}

killAndScore :: (Killable a) => [a] -> Score -> Score -> ([a], Score)
killAndScore [] _ score = ([], score)
killAndScore (x : xs) (Score points) (Score score) | isAlive x = (x : s, newScore)
                                                   | otherwise = killAndScore xs (Score points) (Score $ score + points)
    where
        (s, newScore) = killAndScore xs (Score points) (Score score)

updatePlayer :: Time -> Player -> CollisionList -> Player
updatePlayer (Time dt) player collisionList = Player {
    playerID = playerID player,
    playerPosition = newPosition,
    playerVelocity = playerVelocity player,
    playerBoxCollider = BoxCollider {
            boxColliderSize = boxColliderSize $ playerBoxCollider player,
            boxColliderPosition = newPosition
        },
    playerHealth = Health $ max (health - damage) 0,
    playerCooldown = if cooldown > 0 then Time (cooldown - dt) else Time 0,
    playerInvulnerableTime = if invulnerableTime > 0 then Time (invulnerableTime - dt) else
       (if damage > 0 then playerInvulnerableTimeout else Time 0)
}
    where
        Position position = playerPosition player
        Velocity velocity = playerVelocity player
        newPosition = Position $ limitVector2 limitPosition_ld limitPosition_ru $ position + velocity * (Vector2 dt dt)
        Time invulnerableTime = playerInvulnerableTime player
        Damage damage | invulnerableTime > 0 = Damage 0
                      | not $ isEmpty $ collisionListPlayerAsteroids collisionList = asteroidDamage
                      | not $ isEmpty $ collisionListPlayerEnemy collisionList = enemyDamage
                      | not $ isEmpty $ collisionListPlayerBullet collisionList = bulletDamage
                      | otherwise = Damage 0
        Health health = playerHealth player
        Time cooldown = playerCooldown player

updateAsteroid :: Time -> [(ID, ID)] -> Asteroid -> Maybe Asteroid
updateAsteroid (Time dt) collisionList asteroid
    | vector2y position > vector2y limitPosition_ld = Just Asteroid {
        asteroidID = asteroidID asteroid,
        asteroidPosition = newPosition,
        asteroidRotation = Rotation $ rotation + rotationSpeed * dt,
        asteroidVelocity = Velocity velocity,
        asteroidHealth = Health $ health - damage,
        asteroidBoxCollider = BoxCollider {
            boxColliderSize = boxColliderSize $ asteroidBoxCollider asteroid,
            boxColliderPosition = newPosition
        }
    }
    | otherwise = Nothing
    where
        Position position = asteroidPosition asteroid
        Velocity velocity = asteroidVelocity asteroid
        Rotation rotation = asteroidRotation asteroid
        Rotation rotationSpeed = asteroidRotationSpeed
        Health health = asteroidHealth asteroid
        newPosition = Position $ position + velocity * (Vector2 dt dt)
        Damage damage | case lookup (asteroidID asteroid) collisionList of
            Nothing -> False
            _ -> True = bulletDamage
                      | otherwise = Damage 0

updateAsteroids :: Time -> Score -> [Asteroid] -> [(ID, ID)] -> ([Asteroid], Score)
updateAsteroids time score asteroids collisionList = killAndScore justAsteroids asteroidPoints score
    where
        maybeAsteroids = map (updateAsteroid time collisionList) asteroids
        justAsteroids = unwrapFilterJust maybeAsteroids

updateEnemy :: Time -> [(ID, ID)] -> Enemy -> Maybe Enemy
updateEnemy (Time dt) collisionList enemy
    | vector2y position > vector2y limitPosition_ld = Just Enemy {
        enemyID = enemyID enemy,
        enemyPosition = newPosition,
        enemyVelocity = Velocity velocity,
        enemyHealth = Health $ health - damage,
        enemyCooldown = if cooldown > 0 then Time (cooldown - dt) else enemyReloadTime,
        enemyBoxCollider = BoxCollider {
            boxColliderSize = boxColliderSize $ enemyBoxCollider enemy,
            boxColliderPosition = newPosition
        }
    }
    | otherwise = Nothing
    where
        Position position = enemyPosition enemy
        Velocity velocity = enemyVelocity enemy
        Health health = enemyHealth enemy
        newPosition = Position $ position + velocity * (Vector2 dt dt)
        Damage damage | case lookup (enemyID enemy) collisionList of
            Nothing -> False
            _ -> True = bulletDamage
                      | otherwise = Damage 0
        Time cooldown = enemyCooldown enemy

updateEnemies :: Time -> Score -> [Enemy] -> [(ID, ID)] -> ([Enemy], Score)
updateEnemies time score enemies collisionList = killAndScore justEnemies enemyPoints score
    where
        maybeEnemies = map (updateEnemy time collisionList) enemies
        justEnemies = unwrapFilterJust maybeEnemies

updateBullet :: Time -> [(ID, ID)] -> Bullet -> Maybe Bullet
updateBullet (Time dt) collisionList bullet
    | damage > 0 = Nothing
    | vector2y position > vector2y limitPosition_ru || vector2y position < vector2y limitPosition_ld = Nothing
    | otherwise = Just Bullet {
        bulletID = bulletID bullet,
        bulletPosition = newPosition,
        bulletVelocity = Velocity velocity,
        bulletBoxCollider = BoxCollider {
            boxColliderSize = boxColliderSize $ bulletBoxCollider bullet,
            boxColliderPosition = newPosition
        }
    }
    where
        Position position = bulletPosition bullet
        Velocity velocity = bulletVelocity bullet
        newPosition = Position $ position + velocity * (Vector2 dt dt)
        Damage damage | case lookup (bulletID bullet) (map
            (\(x, y) -> (y, x)) collisionList) of
            Nothing -> False
            _ -> True = Damage 1
                      | otherwise = Damage 0

updateBullets :: Time -> [Bullet] -> [(ID, ID)] -> [Bullet]
updateBullets time bullets collisionList = unwrapFilterJust $ map
    (updateBullet time collisionList) bullets

updateScene :: Time -> Scene -> Scene
updateScene dT@(Time dt) scene = Scene
    {
        sceneGen = gen3,
        sceneID = idnt3,
        sceneTime = newTime,
        scenePlayer = updatePlayer dT player collisionList,
        sceneEnemies = newEnemies ++ updatedEnemies,
        sceneAsteroids = newAsteroids ++ updatedAsteroids,
        scenePlayerBullets = playerBullets,
        sceneEnemyBullets = newEnemyBullets ++ enemyBullets,
        sceneScore = score2
    }
    where
        currentGen = sceneGen scene
        player = scenePlayer scene
        asteroids = sceneAsteroids scene
        idnt = sceneID scene
        time@(Time time_float) = sceneTime scene
        enemies = sceneEnemies scene
        (randomFloat, gen1) = genRandomFloat (5, 10) currentGen
        (randomRot, gen2) = genRandomRotation gen1
        (randomPos, gen3) = genRandomPosition gen2 spawnPosition_ld spawnPosition_ru
        newTime = Time (time_float + dt)
        collisionList = checkCollisions player asteroids enemies
            (scenePlayerBullets scene)
            (sceneEnemyBullets scene)
        (updatedAsteroids, score1) = updateAsteroids dT (sceneScore scene) asteroids (collisionListAsteroidBullet collisionList)
        (updatedEnemies, score2) = updateEnemies dT score1 enemies (collisionListEnemyBullet collisionList)
        playerBullets = updateBullets dT (scenePlayerBullets scene) (collisionListEnemyBullet collisionList ++
                                                                     collisionListAsteroidBullet collisionList)
        enemyBullets = updateBullets dT (sceneEnemyBullets scene) (collisionListPlayerBullet collisionList)
        (ID idnt1_i, newEnemyBullets) = instantiateFromTransformable
            (instantiateBullet $ Velocity $ vector2FromPolar bulletSpeed $ -pi/2) idnt enemyShootOffset $
            filter (\enemy -> enemyCooldown enemy < Time 0) enemies
        (newAsteroids, (ID idnt2_i)) = if activateOnTime (powerIntervals (Time 8) defualtDifficulty 0.0 1.0) time newTime then
            ([instantiateAsteroid
                (Health $ round randomFloat)
                (Size $ Vector2 (7 * randomFloat) (7 * randomFloat))
                randomRot
                (ID idnt1_i)
                randomPos], ID (idnt1_i + 1)) else ([], ID idnt1_i)
        (newEnemies, idnt3) = if activateOnTime (powerIntervals (Time 6) defualtDifficulty 0.5 1.0) time newTime then
            ([instantiateEnemy
                (ID idnt2_i)
                randomPos], ID (idnt2_i + 1)) else ([], ID idnt2_i)
        

sceneChangePlayerVelocity :: Scene -> Velocity -> Scene
sceneChangePlayerVelocity scene velocity = Scene {
        sceneGen = sceneGen scene,
        sceneID = sceneID scene,
        sceneTime = sceneTime scene,
        scenePlayer = Player {
            playerID = playerID player,
            playerPosition = playerPosition player,
            playerVelocity = velocity,
            playerBoxCollider = playerBoxCollider player,
            playerHealth = playerHealth player,
            playerCooldown = playerCooldown player,
            playerInvulnerableTime = playerInvulnerableTime player
        },
        sceneAsteroids = sceneAsteroids scene,
        sceneEnemies = sceneEnemies scene,
        scenePlayerBullets = scenePlayerBullets scene,
        sceneEnemyBullets = sceneEnemyBullets scene,
        sceneScore = sceneScore scene
    }
    where
        player = scenePlayer scene

sceneInstantiatePlayerBullet :: Scene -> Scene
sceneInstantiatePlayerBullet scene = Scene {
        sceneGen = sceneGen scene,
        sceneID = ID (idnt_i + 1),
        sceneTime = sceneTime scene,
        scenePlayer = Player {
            playerID = playerID player,
            playerPosition = playerPosition player,
            playerVelocity = playerVelocity player,
            playerBoxCollider = playerBoxCollider player,
            playerHealth = playerHealth player,
            playerCooldown = playerReloadTime,
            playerInvulnerableTime = playerInvulnerableTime player
        },
        sceneAsteroids = sceneAsteroids scene,
        sceneEnemies = sceneEnemies scene,
        scenePlayerBullets = Bullet {
            bulletID = ID idnt_i,
            bulletPosition = newBulletPosition,
            bulletVelocity = Velocity $ vector2FromPolar bulletSpeed (pi/2),
            bulletBoxCollider = BoxCollider {
                boxColliderPosition = newBulletPosition,
                boxColliderSize = bulletSize
            }
        } : (scenePlayerBullets scene),
        sceneEnemyBullets = sceneEnemyBullets scene,
        sceneScore = sceneScore scene
    }
    where
        player = scenePlayer scene
        Position position = playerPosition player
        Position playerShootV2 = playerShootPosition
        newBulletPosition = Position (position + playerShootV2)
        ID idnt_i = sceneID scene