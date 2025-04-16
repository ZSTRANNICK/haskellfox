module Graphics
    ( drawScene
    ) where

import Graphics.Gloss.Interface.Pure.Game

import Vectors
import Types
import Constants
    (scorePosition, scoreSize, healthPosition, healthSize)

drawScene :: Scene -> Picture
drawScene scene = Pictures $
    (map drawAsteroid $ sceneAsteroids scene) ++
    (map (drawBullet $ makeColorI 200 150 150 255) $ sceneEnemyBullets scene) ++
    (map (drawBullet white) $ scenePlayerBullets scene) ++
    (map drawEnemy $ sceneEnemies scene) ++
    (drawPlayer $ player) ++
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

drawPlayer :: Player -> [Picture]
drawPlayer player 
    | health > 0 = [Translate x y $ (if invulnerableTime > 0 then (Color red) else (Color white))
        $ polygon [(-30, -20), (0, 30), (30, -20)]]
    | otherwise = map ((Color red) . (Translate x y)) [
            polygon [(-35, -20), (-5, 30), (-5, -20)],
            polygon [(35, -20), (5, 30), (5, -20)]
        ]
    where
        Health health = playerHealth player
        Time invulnerableTime = playerInvulnerableTime player
        Position (Vector2 x y) = playerPosition player

drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid = Translate x y $ Color (makeColorI 100 100 100 255) $ Rotate r $ polygon $
    map (\(x_p, y_p) -> (x_p * x_size, y_p * x_size)) [(-1, -1), (-1, 1), (1, 1), (1, -1)]
    where
        Position (Vector2 x y) = asteroidPosition asteroid
        Rotation r = asteroidRotation asteroid
        Size (Vector2 x_size _) = boxColliderSize $ asteroidBoxCollider asteroid

drawBullet :: Color -> Bullet -> Picture
drawBullet bColor bullet = Translate x y $ Color bColor $ polygon [(-5, -10), (-5, 10), (5, 10), (5, -10)]
    where
        Position (Vector2 x y) = bulletPosition bullet

drawEnemy :: Enemy -> Picture
drawEnemy enemy = Translate x y $ Color (makeColorI 200 150 150 255) $ polygon [(-30, 20), (0, -30), (30, 20)]
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