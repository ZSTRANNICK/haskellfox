module EventHandler
    ( run
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics
import System.Random(newStdGen)

import Vectors
import Types
import Lib
import Constants

handleInput :: Event -> Scene -> Scene
handleInput event scene
    | health > 0 = handleInputAlive event scene
    | otherwise = handleInputDead event scene
    where
        Health health = playerHealth $ scenePlayer scene

handleInputAlive :: Event -> Scene -> Scene
handleInputAlive (EventKey (Char 'a') Down _ _) scene = sceneChangePlayerVelocity scene $
    Velocity $ vector2FromPolar defaultPlayerSpeed $ pi
handleInputAlive (EventKey (Char 'd') Down _ _) scene = sceneChangePlayerVelocity scene $
    Velocity $ vector2FromPolar defaultPlayerSpeed $ 0
handleInputAlive (EventKey (Char 'a') Up _ _) scene
    | velocity_v2_x < 0 = sceneChangePlayerVelocity scene $ Velocity $ Vector2 0 0
    | otherwise = sceneChangePlayerVelocity scene $ velocity
        where
            velocity = playerVelocity $ scenePlayer scene
            Velocity (Vector2 velocity_v2_x _) = velocity
handleInputAlive (EventKey (Char 'd') Up _ _) scene
    | velocity_v2_x > 0 = sceneChangePlayerVelocity scene $ Velocity $ Vector2 0 0
    | otherwise = sceneChangePlayerVelocity scene $ velocity
        where
            velocity = playerVelocity $ scenePlayer scene
            Velocity (Vector2 velocity_v2_x _) = velocity
handleInputAlive (EventKey (SpecialKey KeySpace) Down _ _) scene
    | cooldown > 0 = scene
    | otherwise = sceneInstantiatePlayerBullet scene
        where
            Time cooldown = playerCooldown $ scenePlayer scene
handleInputAlive _ scene = scene

handleInputDead :: Event -> Scene -> Scene
handleInputDead (EventKey (Char 'r') Down _ _) scene = defaultScene $ sceneGen scene
handleInputDead _ scene = sceneChangePlayerVelocity scene $ Velocity $ Vector2 0 0

run :: IO ()
run = do
    gen <- newStdGen
    playerImage <- loadBMP "./Textures/Player.bmp"
    playerDamagedImage <- loadBMP "./Textures/PlayerDamaged.bmp"
    playerDeadImage <- loadBMP "./Textures/PlayerDead.bmp"
    enemyImage <- loadBMP "./Textures/Enemy.bmp"
    enemyPlayerImage <- loadBMP "./Textures/PlayerBullet.bmp"
    enemyBulletImage <- loadBMP "./Textures/EnemyBullet.bmp"
    asteroidImage <- loadBMP "./Textures/Asteroid.bmp"
    play display bgColor steps (defaultScene gen) (drawScene [playerImage, playerDamagedImage, playerDeadImage, enemyImage, enemyPlayerImage, enemyBulletImage, asteroidImage]) handleInput $ updateScene . Time
        
