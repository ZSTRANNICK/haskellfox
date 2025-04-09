module Constants
    ( defaultVelocity
    , playerReloadTime
    , playerInvulnerableTimeout
    , defaultPlayer
    , defaultPlayerSpeed
    , playerShootPosition
    , asteroidDamage
    , asteroidRotationSpeed
    , defaultEnemyHealth
    , enemyDamage
    , enemyReloadTime
    , enemyShootOffset
    , enemySize
    , bulletDamage
    , bulletSpeed
    , bulletSize
    , defualtDifficulty
    , defaultScene
    , limitPosition_ld
    , limitPosition_ru
    , spawnPosition_ld
    , spawnPosition_ru
    , enemyPoints
    , asteroidPoints
    , scorePosition
    , scoreSize
    , healthPosition
    , healthSize
    , steps
    , bgColor
    , display
    ) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random(StdGen)

import Vectors
import Types

-- [CONSTANTS] --

defaultVelocity :: Velocity
defaultVelocity = Velocity $ Vector2 {vector2x = 0, vector2y = -70}

playerReloadTime :: Time
playerReloadTime = Time 0.7

playerInvulnerableTimeout :: Time
playerInvulnerableTimeout = Time 1.0

playerShootPosition :: Position
playerShootPosition = Position $ Vector2 {vector2x = 0, vector2y = 30}

asteroidDamage :: Damage
asteroidDamage = Damage 2

asteroidRotationSpeed :: Rotation
asteroidRotationSpeed = Rotation 5

defaultEnemyHealth :: Health
defaultEnemyHealth = Health 1

enemyDamage :: Damage
enemyDamage = Damage 1

enemyReloadTime :: Time
enemyReloadTime = Time 1.5

enemyShootOffset :: Position
enemyShootOffset = Position $ Vector2 0 (-30)

enemySize :: Size
enemySize = Size $ Vector2 30 20

bulletDamage :: Damage
bulletDamage = Damage 1

bulletSpeed :: Float
bulletSpeed = 350.0

bulletSize :: Size
bulletSize = Size $ Vector2 10 10

limitPosition_ld :: Vector2
limitPosition_ld = Vector2 (-300) (-600)

limitPosition_ru :: Vector2
limitPosition_ru = Vector2 300 700

spawnPosition_ld :: Position
spawnPosition_ld = Position $ Vector2 (-300) 600

spawnPosition_ru :: Position
spawnPosition_ru = Position $ Vector2 300 650

enemyPoints :: Score
enemyPoints = Score 5

asteroidPoints :: Score
asteroidPoints = Score 10

defaultPlayer :: Player
defaultPlayer = Player
    {
        playerID = ID 0,
        playerPosition = Position $ Vector2 0 (-400),
        playerVelocity = Velocity $ Vector2 0 0,
        playerBoxCollider = BoxCollider
        {
            boxColliderSize = Size $ Vector2 30 20,
            boxColliderPosition = Position $ Vector2 0 (-400)
        },
        playerHealth = Health 3,
        playerCooldown = Time 0.0,
        playerInvulnerableTime = Time 0.0
    }

defaultPlayerSpeed :: Float
defaultPlayerSpeed = 300.0

defualtDifficulty :: Float
defualtDifficulty = 1.2

defaultScene :: StdGen -> Scene
defaultScene gen = Scene {
    sceneGen = gen,
    sceneID = ID 3,
    sceneTime = Time 0,
    scenePlayer = defaultPlayer,
    sceneAsteroids = [],
    sceneEnemies = [],
    scenePlayerBullets = [],
    sceneEnemyBullets = [],
    sceneScore = Score 0
}

scorePosition :: Position
scorePosition = Position $ Vector2 150 400

scoreSize :: Size
scoreSize = Size $ Vector2 0.25 0.25

healthPosition :: Position
healthPosition = Position $ Vector2 (-300) 400

healthSize :: Size
healthSize = Size $ Vector2 0.25 0.25

windowWidth :: Int
windowWidth = 700

windowHeight :: Int
windowHeight = 900

display :: Display
display = InWindow "Game" (windowWidth, windowHeight) (50, 50)

bgColor :: Color
bgColor = black

steps :: Int
steps = 60