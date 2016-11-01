{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Sprite

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        player          :: Player,
        asteroids        :: [Asteroid],
        bullets        :: [Bullet]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

initial :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot newPlayer [newAsteroid] []

---------------------------------------------------  CLASS DEFINITIONS  -----------------------------------------------------
class Moveable m where
    getPosition :: m -> Vector
    addSpeed :: m -> m
    step :: World -> m -> m

class Drawable d where
    drawMe :: d -> Picture

-- PLAYER DEFINITIONS
data Player = Player {  position :: Vector,
                        speed :: Vector,
                        rotation :: Float,
                        sprite :: Sprite }
-- player init
newPlayer :: Player
newPlayer = Player (0, 0) (0, 0) 0 playerSprite 

-- player draw event
instance Drawable Player where
    drawMe p@Player{position, rotation, sprite} = drawSprite sprite position rotation

-- player step event
instance Moveable Player where
    getPosition Player{position} = position
    addSpeed p@Player{position, speed}   = p{position = position + speed}
    step World{rotateAction, movementAction, shootAction} p@Player{speed, rotation} = 
        addSpeed p 
        { 
            speed = mulSV 0.98 (speed + acceleration movementAction),
            rotation = rotation + rotateDirection rotateAction
        }
        where   rotateDirection NoRotation = 0
                rotateDirection RotateLeft = -2
                rotateDirection RotateRight = 2
                acceleration NoMovement = (0, 0)
                acceleration Thrust = rotateV (-rotation * (3.14156982/ 180)) (0.175, 0)


-- ASTEROID DEFINITIONS
data Asteroid = Asteroid {positionAs :: Vector,
                          speedAs    :: Vector,
                          rotationAs :: Float,
                          spriteAs   :: Sprite
                          }

-- asteriod init
newAsteroid :: Asteroid
newAsteroid = Asteroid (5, 5) (0.5, 0.5) 0 (asteroidSprite 5)

-- asteroid sprite
asteroidSprite :: Int -> Sprite
asteroidSprite vertices = asteroidVertex vertices vertices

-- makes a list of vertices in a circle
asteroidVertex :: Int -> Int -> [Vector]
asteroidVertex 0 vertices = [(1, 0)]
asteroidVertex i vertices = rotateV (2 * 3.1415 * (fromIntegral  i) / (fromIntegral vertices)) (1, 0) : asteroidVertex (i-1) vertices

instance Drawable Asteroid where
    drawMe a@Asteroid{positionAs, rotationAs, spriteAs} = drawSprite spriteAs positionAs rotationAs

instance Moveable Asteroid where
    getPosition Asteroid{positionAs} = positionAs
    addSpeed a@Asteroid{positionAs, speedAs} = a{positionAs = positionAs + speedAs}
    step _ a = addSpeed a

-- BULLET DEFINITIONS
data Bullet = Bullet {  positionBu :: Vector,
                        speedBu :: Vector,
                        rotationBu :: Float,
                        spriteBu :: Sprite
                }
-- bullet init
newBullet :: Vector -> Vector -> Float -> Bullet
newBullet pos spd dir = Bullet pos spd dir bulletSprite

-- bullet sprite
bulletSprite :: Sprite
bulletSprite = [(-1, 0), (1, 0)]

instance Drawable Bullet where
    drawMe a@Bullet{positionBu, rotationBu, spriteBu} = drawSprite spriteBu positionBu rotationBu

instance Moveable Bullet where
    getPosition Bullet{positionBu} = positionBu
    addSpeed b@Bullet{positionBu, speedBu} = b{positionBu = positionBu + speedBu}
    step _ a = addSpeed a