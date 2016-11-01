{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

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
        asteroid        :: Asteroid
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot


-- sprite definitions
type Sprite = [Point]


drawSprite :: Sprite -> Picture
drawSprite spr = Color (makeColor 1 1 1 1) (Scale 10 10 (Line spr))

initial :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot newPlayer newAsteroid


--------------------------------------------------- CLASS DEFINITIONS --------------------------------------------------
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

-- player sprite
playerSprite :: Sprite
playerSprite = [(-1, 1), (1, 0), (-1, -1), (-0.5, 0), (-1, 1)]

-- player draw event
instance Drawable Player where
    drawMe p@Player{position, rotation, sprite} = Translate (fst position) (snd position) (Rotate rotation (drawSprite sprite))

-- player step event
instance Moveable Player where
    getPosition Player{position} = position
    addSpeed p@Player{position, speed}   = p{position = position + speed}
    step World{rotateAction, movementAction} p@Player{speed, rotation} = 
        p { 
            speed = mulSV 0.98 (speed + acceleration movementAction),
            rotation = rotation + rotateDirection rotateAction
        }
              where rotateDirection NoRotation = 0
                    rotateDirection RotateLeft = -2
                    rotateDirection RotateRight = 2
                    acceleration NoMovement = (0, 0)
                    acceleration Thrust = rotateV (-rotation * (3.14156982/ 180)) (0.175, 0)

-- ASTEROID DEFINITIONS

data Asteroid = Asteroid {positionAs :: Vector,
                          speedAs :: Vector,
                          rotationAs :: Float,
                          spriteAs :: Sprite
                          }

-- asteriod init
newAsteroid :: Asteroid
newAsteroid = Asteroid (5, 5) (0.5, 0.5) 0 (asteroidSprite 5)

-- asteroid sprite
asteroidSprite :: Int -> Sprite
asteroidSprite vertices = asteroidVertex vertices vertices

asteroidVertex :: Int -> Int -> [Vector]
asteroidVertex 0 vertices = [(1, 0)]
asteroidVertex i vertices = rotateV (2 * 3.1415 * (fromIntegral  i) / (fromIntegral vertices)) (1, 0) : asteroidVertex (i-1) vertices


instance Drawable Asteroid where
    drawMe a@Asteroid{positionAs, rotationAs, spriteAs} = Translate (fst positionAs) (snd positionAs) (Rotate rotationAs (drawSprite spriteAs))

instance Moveable Asteroid where
    getPosition Asteroid{positionAs} = positionAs
    addSpeed a@Asteroid{positionAs, speedAs} = a{positionAs = positionAs + speedAs}
    step _ a = a