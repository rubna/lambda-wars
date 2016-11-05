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
        state            :: GameState,
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        shootTimer       :: Float,
        player           :: Player,
        asteroids        :: [Asteroid],
        bullets          :: [Bullet],
        stars            :: [Star],
        particles        :: [Particle]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot
    deriving Eq
data GameState      = Playing    | Dead
    deriving Eq

initial :: Int -> World
initial seed = World (mkStdGen seed) Playing NoRotation NoMovement DontShoot 1 newPlayer [] [] [] []

randomRange :: World -> (Float, Float) -> ([Float], World)
randomRange w@World{rndGen} range = (result, w{rndGen = newSeed})
                where result = randomRs range rndGen
                      newSeed = mkStdGen (head (randoms rndGen))

--------------------------------------------------- GLOBAL EVENTS ----------------------------------------------------
-- code for checking collision between bullets and asteroids
shootAsteroids :: World -> World
shootAsteroids w@World{bullets, asteroids} = w{asteroids = snd newLists,
                                               bullets   = fst newLists}
                                            where newLists = doHitting ([], []) (bullets, asteroids)

doHitting :: ([Bullet], [Asteroid]) -> ([Bullet], [Asteroid]) -> ([Bullet], [Asteroid])
doHitting (clearBullets, clearAsteroids) (bs, []) = (clearBullets ++ bs, clearAsteroids)  --we've checked for all asteroids
doHitting (clearBullets, clearAsteroids) ([], a:as) = doHitting ([], clearAsteroids ++ [a]) (clearBullets, as) --this asteroid wasn't hit! phew
doHitting (clearBullets, clearAsteroids)  ((b:bs), (a:as)) | checkBulletCollision b a = doHitting ([], []) (clearBullets ++ bs, clearAsteroids ++ (explodeAsteroid a 0) ++ as) -- hit!! check next asteroid, leaving the bullet and the current asteroid out
                                                           | otherwise          = doHitting(clearBullets ++ [b], clearAsteroids) (bs, a:as) -- this bullet didn't hit the asteroid~! check next bullet

-- splits an asteroid in two smaller ones if its size > 1
explodeAsteroid :: Asteroid -> Float -> [Asteroid]
explodeAsteroid Asteroid{positionAs, sizeAs} hitDir | sizeAs == 1 = []
                                                    | otherwise   = [newAsteroid positionAs (toCartesian (1, hitDir + 90)) (sizeAs - 1), 
                                                                     newAsteroid positionAs (toCartesian (1, hitDir - 90)) (sizeAs - 1)]

-- checks if there's collision between a Bullet and an Asteroid
checkBulletCollision :: Bullet -> Asteroid -> Bool
checkBulletCollision a b = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

checkKilled :: World -> World
checkKilled w@World{player, asteroids} | any (==True) (map (checkPlayerCollision player) asteroids) = 
                                                emitParticles 20 (getPosition player) 3 w{state = Dead}
                                       | otherwise = w

-- checks if Player collided with an Asteroid
checkPlayerCollision :: Player -> Asteroid -> Bool
checkPlayerCollision a b = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

-- player shooting
shoot :: Player -> World -> World
shoot Player{position, rotation} w@World{bullets, shootAction, shootTimer} = 
            w{  bullets = maybeBullet ++ bullets,
                shootTimer = newShootTimer}
              where shot = shootAction == Shoot && shootTimer == 1
                    maybeBullet   | shot = [newBullet (position + toCartesian (10, rotation)) rotation]
                                  | otherwise = []
                    newShootTimer | shot = 0
                                  | otherwise = min 1 (shootTimer + 0.05)

-- player trail particles
playerTrail :: World -> World
playerTrail w@World{player, movementAction} = trail movementAction
                where trail Thrust = emitParticles 1 (getPosition player - toCartesian(4, getRotation player)) 1 w
                      trail NoMovement = w

-- emit a particle at a certain place
emitParticles :: Int -> Vector -> Float -> World -> World
emitParticles n pos maxSpd w@World{particles} = 
        newWorld{
            particles = (makeParticles n []) ++ particles
         }
         where  rnds = randomRange w (0, 1) -- make infinite list of random floats between 0 and 1, and a new World
                floats = fst rnds           -- take infinite list of floats out of the tuple
                newWorld = snd rnds         -- the new world
                
                makeParticles :: Int -> [Particle] -> [Particle]
                makeParticles 0 ps = ps
                makeParticles i ps = (newParticle pos (spd i)) : makeParticles (i - 1) ps
                spd j = toCartesian ((floats !! (j * 2)) * maxSpd, (floats !! (j * 2 + 1)) * 360.0)  -- the random speed


-- time out Bullets, Particles
timeOutInstances :: World -> World
timeOutInstances w@World{bullets, particles} = w{bullets = concatMap timeOutBullet bullets,
                                               particles = concatMap timeOutParticle particles}

-- delete a bullet if it's timed out
timeOutBullet :: Bullet -> [Bullet]
timeOutBullet b@Bullet{destroyTimer} | destroyTimer < 1.0 = [b]
                                     | otherwise         = []
-- delete a particle if it's going too slow
timeOutParticle :: Particle -> [Particle]
timeOutParticle p@Particle{} | (magV $ getSpeed p) > 0.1 = [p]
                             | otherwise         = []


-- start game handler
startGame :: World -> World
startGame w@World{shootAction} | shootAction == Shoot = w{state = Playing, shootTimer = 0, player = newPlayer}
                               | otherwise = w

---------------------------------------------------  CLASS DEFINITIONS  -----------------------------------------------------
class Moveable m where
    getPosition :: m -> Vector
    setPosition :: m -> Vector -> m
    getRadius :: m -> Float
    getSpeed :: m -> Vector
    getRotation :: m -> Float
    getRotation m = argV $ getSpeed m
    addSpeed :: m -> m                  -- add speed to position
    addSpeed m = setPosition m (getPosition m + getSpeed m)
    wrap :: m -> m                      -- wraps around a moveable
    wrap m = setPosition m (wrapVector (getPosition m) (-205) 205 (205) (-205))
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
    getSpeed Player{speed} = speed
    setPosition p@Player{position} newPos = p{position = newPos}
    getRadius Player{radius} = radius
    getRotation Player{rotation} = rotation
    step World{rotateAction, movementAction, shootAction} p@Player{speed, rotation} = 
        wrap $ addSpeed p 
        { 
            speed = mulSV 0.975 (speed + acceleration movementAction),
            rotation = rotation + rotateDirection rotateAction
        }
        where   rotateDirection NoRotation = 0
                rotateDirection RotateLeft = -4
                rotateDirection RotateRight = 4
                acceleration NoMovement = (0, 0)
                acceleration Thrust = toCartesian (0.175, rotation)

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

-- spawn asteroids
spawnAsteroids :: Int -> World -> World
spawnAsteroids n w@World{asteroids, player} = 
    newWorld{ asteroids = asteroids ++ updateAsteroid 0 blankAsteroids }
        where   blankAsteroids = replicate n (newAsteroid (0, 0) (0, 0) 3)            -- initialize list of asteroids
                positions = zip (map (*400) (drop n floats)) (map (*300) floats)                     -- make infinite list of random positions 
                speeds = map toCartesian $ (zip (drop (n*2) floats)) (map (*180) (drop (n*3) floats)) -- make speed vectors from random polar vectors

                rnds = (randomRange w (-1, 1))        -- make infinite list of random floats between -1 and 1, and a new World
                floats = fst rnds                     -- take infinite list of floats out of the tuple
                newWorld = snd rnds                   -- the new world
                -- update asteroids
                updateAsteroid :: Int -> [Asteroid] -> [Asteroid]
                updateAsteroid _ [] = []
                updateAsteroid i (a@Asteroid{positionAs, speedAs}:as)  | magV (positions !! i - getPosition player) < 100 = updateAsteroid (i+1) (a:as)
                                                                       | otherwise = a{  positionAs = positions !! i, 
                                                                                          speedAs = speeds !! i       } : updateAsteroid (i+1) as

instance Drawable Asteroid where
    drawMe a@Asteroid{positionAs, rotationAs, spriteAs} = drawSprite spriteAs positionAs (getRadius a) rotationAs

instance Moveable Asteroid where
    getPosition Asteroid{positionAs} = positionAs
    setPosition a@Asteroid{positionAs} newPos = a{positionAs = newPos}
    getSpeed a@Asteroid{speedAs} = speedAs
    getRadius Asteroid{sizeAs} = fromIntegral sizeAs * 5.0
    step _ a = wrap $ addSpeed a


wrapVector :: Vector -> Float -> Float -> Float -> Float -> Vector
wrapVector (x, y) left right upper lower | x < left  = (x + width, y)
                                         | y > upper = (x, y - height)
                                         | x > right = (x - width, y)
                                         | y < lower = (x, y + height)
                                         | otherwise = (x, y)
                                            where width  = right - left
                                                  height = upper - lower

-- BULLET DEFINITIONS
data Bullet = Bullet {  positionBu  :: Vector,
                        speedBu     :: Vector,
                        rotationBu  :: Float,
                        radiusBu    :: Float,
                        destroyTimer:: Float,
                        spriteBu    :: Sprite
                     }
-- bullet init
newBullet :: Vector -> Float -> Bullet
newBullet pos dir = Bullet pos (toCartesian (8, dir)) dir 2 0 bulletSprite

instance Drawable Bullet where
    drawMe a@Bullet{positionBu, rotationBu, spriteBu} = drawSprite spriteBu positionBu 7.5 rotationBu

instance Moveable Bullet where
    getPosition Bullet{positionBu} = positionBu
    setPosition b@Bullet{positionBu} newPos = b{positionBu = newPos}
    getSpeed Bullet{speedBu} = speedBu
    getRadius Bullet{radiusBu} = radiusBu
    step _ b@Bullet{destroyTimer} = wrap $ addSpeed b{destroyTimer = destroyTimer + 0.02}

-- STAR DEFINITIONS
data Star = Star {  positionSt  :: Vector,
                    z           :: Float,
                    speedSt     :: Vector,
                    spriteSt    :: Sprite}

instance Moveable Star where
    getPosition Star{positionSt} = positionSt
    setPosition s@Star{positionSt} newPos = s{positionSt = newPos}
    getSpeed Star{speedSt}  = speedSt
    getRadius Star{z}       = 2 / z
    step World{player} s@Star{speedSt, z}    = wrap $ addSpeed s{speedSt = mulSV (-1/(z*z)) (getSpeed player)}

instance Drawable Star where
    drawMe s@Star{positionSt, spriteSt, z} = drawSprite spriteSt positionSt (2.5 / z) 0

--init
newStar :: Vector -> Float -> Star
newStar pos z = Star pos z (0, 0) starSprite

-- spawn stars
spawnStars :: Int -> World -> World
spawnStars n w@World{stars} = 
    newWorld{ stars = stars ++ updateStars 0 blankStars }
        where   blankStars = replicate n (newStar (0, 0) 2)            -- initialize list of stars

                rnds = (randomRange w (-205, 205))        -- make infinite list of random floats between -205 and 205, and a new World
                newWorld = snd rnds                       -- the new world out of the tuple
                floats = fst rnds                         -- take infinite list of floats out of the tuple
                positions = zip (drop n floats) (floats)  -- make infinite list of random Vectors 
                zs     = map ((*2) . (+2.1).(/205)) (drop (n*2) floats) --make an infinite list of floats between ~1.2 and ~8 (for the z coordinate)
                -- update star positions with random positions
                updateStars :: Int -> [Star] -> [Star]
                updateStars _ [] = []
                updateStars i (s@Star{positionSt, z}:ss)  = s{  positionSt = positions !! i, z = zs !! i} : updateStars (i+1) ss

--  PARTICLE DEFINITIONS
data Particle = Particle {  positionPa  :: Vector,
                            speedPa     :: Vector}

newParticle :: Vector -> Vector -> Particle
newParticle pos spd = Particle pos spd

instance Moveable Particle where
    getPosition Particle{positionPa} = positionPa
    setPosition s@Particle{positionPa} newPos = s{positionPa = newPos}
    getSpeed Particle{speedPa} = speedPa
    getRadius _ = 3
    step _ p@Particle{speedPa} = addSpeed p{speedPa = mulSV 0.95 speedPa}

instance Drawable Particle where
    drawMe p@Particle{positionPa} = drawSprite starSprite positionPa 1 0 -- Translate x y (Color white (Circle getRadius p))