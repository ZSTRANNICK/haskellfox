module Graphics
    ( drawScene
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap (BitmapData)

import Vectors
import Types
import Constants
    (scorePosition, scoreSize, healthPosition, healthSize)

drawScene :: [Picture] -> Scene -> Picture
drawScene [playerImage
    , playerDamagedImage
    , playerDeadImage
    , enemyImage
    , playerBulletImage
    , enemyBulletImage
    , asteroidImage
    ] scene = Pictures $
    (map (drawAsteroid asteroidImage) $ sceneAsteroids scene) ++
    (map (drawBullet (makeColorI 200 150 150 255) enemyBulletImage) $ sceneEnemyBullets scene) ++
    (map (drawBullet white playerBulletImage) $ scenePlayerBullets scene) ++
    (map (drawEnemy enemyImage) $ sceneEnemies scene) ++
    (drawPlayer player playerImage playerDamagedImage playerDeadImage) ++
    [drawScore $ score,
    drawHealth $ playerHealth $ player] ++
    (drawLoseScreen player score)
    where
        player = scenePlayer scene
        score = sceneScore scene

drawLoseScreen :: Player -> Score -> [Picture]
drawLoseScreen player (Score score)
    | health > 0 = []
    | otherwise =
    [
        Color white $ polygon [(-200, -100), (-200, 100), (200, 100), (200, -100)],
        Translate (-150) 40 $ Scale 0.4 0.4 $ Text "You've lost!",
        Translate (-170) (-20) $ Scale 0.3 0.3 $ Text $ "Your score: " ++ (show score),
        Translate (-170) (-80) $ Scale 0.25 0.25 $ Text "Press [r] to restart"
    ]
    where
        Health health = playerHealth player

drawPlayer :: Player -> Picture -> Picture -> Picture -> [Picture]
drawPlayer player playerImage playerDamagedImage playerDeadImage
    | health > 0 = [Translate x y $ scale 5 5 $ (if invulnerableTime > 0 then playerDamagedImage else playerImage)]
    | otherwise = map ((Translate x y) . (scale 5 5)) [playerDeadImage]
    where
        Health health = playerHealth player
        Time invulnerableTime = playerInvulnerableTime player
        Position (Vector2 x y) = playerPosition player

drawAsteroid :: Picture -> Asteroid -> Picture
drawAsteroid asteroidImage asteroid = Translate x y $ Color (makeColorI 100 100 100 255) $ Rotate r $ scale
    (0.07 * x_size) (0.07 * x_size) asteroidImage
    where
        Position (Vector2 x y) = asteroidPosition asteroid
        Rotation r = asteroidRotation asteroid
        Size (Vector2 x_size _) = boxColliderSize $ asteroidBoxCollider asteroid

drawBullet :: Color -> Picture -> Bullet -> Picture
drawBullet bColor bulletPicture bullet = Translate x y $ Color bColor $ scale 4 4 $ bulletPicture
    where
        Position (Vector2 x y) = bulletPosition bullet

drawEnemy :: Picture -> Enemy -> Picture
drawEnemy enemyImage enemy = Translate x y $ scale 5 5 enemyImage
    where
        Position (Vector2 x y) = enemyPosition enemy

drawScore :: Score -> Picture
drawScore (Score score) = Translate x y $ Color white $ Scale scale_x scale_y $ Text $ "Score: " ++ (show score)
    where
        Position (Vector2 x y) = scorePosition
        Size (Vector2 scale_x scale_y) = scoreSize

drawHealth :: Health -> Picture
drawHealth (Health health) = Translate x y $ Color white $ Scale scale_x scale_y $ Text $ "Health: " ++ (show health)
    where
        Position (Vector2 x y) = healthPosition
        Size (Vector2 scale_x scale_y) = healthSize