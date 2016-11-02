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
        shootTimer       :: Float,
        player          :: Player,
        asteroids        :: [Asteroid],
        bullets        :: [Bullet]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot
    deriving Eq

initial :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot 1 newPlayer [newAsteroid (10, 10) (0.5, 0.5) 3, newAsteroid (-100, 50) (-0.5, 0.5) 2] []

--------------------------------------------------- GLOBAL EVENTS ----------------------------------------------------
shootAsteroids :: World -> World
shootAsteroids w@World{bullets, asteroids} = w{asteroids = snd newLists,
                                               bullets   = fst newLists}
                                            where newLists = doHitting ([], []) (bullets, asteroids)

doHitting :: ([Bullet], [Asteroid]) -> ([Bullet], [Asteroid]) -> ([Bullet], [Asteroid])
doHitting (clearBullets, clearAsteroids) (bs, []) = (clearBullets ++ bs, clearAsteroids)  --we've checked for all asteroids
doHitting (clearBullets, clearAsteroids) ([], a:as) = doHitting ([], clearAsteroids ++ [a]) (clearBullets, as) --this asteroid wasn't hit! phew
doHitting (clearBullets, clearAsteroids)  ((b:bs), (a:as)) | checkCollision b a = doHitting ([], explodeAsteroid a) (clearBullets ++ bs, clearAsteroids ++ as) -- hit!! check next asteroid, leaving the bullet and the current asteroid out
                                                           | otherwise = doHitting(clearBullets ++ [b], clearAsteroids ++ [a]) (bs, as) -- this bullet didn't hit the asteroid~! check next bullet

checkCollision :: Bullet -> Asteroid -> Bool
checkCollision b a = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

explodeAsteroid :: Asteroid -> [Asteroid]
explodeAsteroid Asteroid{positionAs, sizeAs} | sizeAs == 1 = []
                                             | otherwise = [newAsteroid positionAs (-0.5, 0.2) (sizeAs - 1), newAsteroid positionAs (0.5, -0.1) (sizeAs - 1)]

---------------------------------------------------  CLASS DEFINITIONS  -----------------------------------------------------
class Moveable m where
    getPosition :: m -> Vector
    getRadius :: m -> Float
    addSpeed :: m -> m
    step :: World -> m -> m

class Drawable d where
    drawMe :: d -> Picture

-- PLAYER DEFINITIONS
data Player = Player {  position :: Vector,
                        speed :: Vector,
                        rotation :: Float,
                        radius :: Float,
                        sprite :: Sprite}
-- player init
newPlayer :: Player
newPlayer = Player (0, 0) (0, 0) 0 10 playerSprite

-- player draw event
instance Drawable Player where
    drawMe p@Player{position, rotation, sprite} = drawSprite sprite position 8 rotation

-- player step event
instance Moveable Player where
    getPosition Player{position} = position
    getRadius Player{radius} = radius
    addSpeed p@Player{position, speed}   = p{position = position + speed}
    step World{rotateAction, movementAction, shootAction} p@Player{speed, rotation} = 
        addSpeed p 
        { 
            speed = mulSV 0.98 (speed + acceleration movementAction),
            rotation = rotation + rotateDirection rotateAction
        }
        where   rotateDirection NoRotation = 0
                rotateDirection RotateLeft = -4
                rotateDirection RotateRight = 4
                acceleration NoMovement = (0, 0)
                acceleration Thrust = toCartesian (0.175, rotation)

shoot :: World -> Player -> World
shoot w@World{bullets, shootAction, shootTimer} Player{position, rotation} = w{bullets = maybeBullet ++ bullets,
                                                                               shootTimer = newShootTimer}
                                                            where shot = shootAction == Shoot && shootTimer == 1
                                                                  maybeBullet   | shot = [newBullet position rotation]
                                                                                | otherwise = []
                                                                  newShootTimer | shot = 0
                                                                                | otherwise = min 1 (shootTimer + 0.05)

toCartesian :: Vector -> Vector
toCartesian polar = rotateV (-(snd polar) * (3.14156982/ 180)) (fst polar, 0)

-- ASTEROID DEFINITIONS
data Asteroid = Asteroid {positionAs :: Vector,
                          speedAs    :: Vector,
                          rotationAs :: Float,
                          sizeAs     :: Int,
                          spriteAs   :: Sprite
                          }

-- asteriod init
newAsteroid :: Vector -> Vector -> Int -> Asteroid
newAsteroid pos spd size = Asteroid pos spd 0 size (asteroidSprite 5)

instance Drawable Asteroid where
    drawMe a@Asteroid{positionAs, rotationAs, spriteAs} = drawSprite spriteAs positionAs (getRadius a / 2.0) rotationAs

instance Moveable Asteroid where
    getPosition Asteroid{positionAs} = positionAs
    getRadius Asteroid{sizeAs} = fromIntegral sizeAs * 10.0
    addSpeed a@Asteroid{positionAs, speedAs} = a{positionAs = positionAs + speedAs}
    step _ a = addSpeed a

-- BULLET DEFINITIONS
data Bullet = Bullet {  positionBu  :: Vector,
                        speedBu     :: Vector,
                        rotationBu  :: Float,
                        radiusBu    :: Float,
                        spriteBu    :: Sprite
                     }
-- bullet init
newBullet :: Vector -> Float -> Bullet
newBullet pos dir = Bullet pos (toCartesian (10, dir)) dir 5 bulletSprite

instance Drawable Bullet where
    drawMe a@Bullet{positionBu, rotationBu, spriteBu} = drawSprite spriteBu positionBu 10 rotationBu

instance Moveable Bullet where
    getPosition Bullet{positionBu} = positionBu
    getRadius Bullet{radiusBu} = radiusBu
    addSpeed b@Bullet{positionBu, speedBu} = b{positionBu = positionBu + speedBu}
    step _ a = addSpeed a