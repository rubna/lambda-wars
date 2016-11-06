{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
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
        particles        :: [Particle],
        ufos             :: [Ufo],
        score            :: Int,
        lives            :: Int,
        invincibility    :: Bool,
        level            :: Int,
        multiplier       :: Int,
        highScore        :: Int
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot
    deriving Eq
data GameState      = Playing    | Dead
    deriving Eq

initial :: Int -> World
initial seed = World (mkStdGen seed) Dead NoRotation NoMovement DontShoot 1 newPlayer [] [] [] [] [newUfo (10, -150)] 0 3 True 0 1 0

randomRange :: World -> (Float, Float) -> ([Float], World)
randomRange w@World{rndGen} range = (result, w{rndGen = newSeed})
                where result = randomRs range rndGen
                      newSeed = mkStdGen (head (randoms rndGen))

--------------------------------------------------- GLOBAL EVENTS ----------------------------------------------------
-- code for checking collision between bullets and asteroids
shootAsteroids :: World -> World
shootAsteroids w@World{bullets, asteroids, score, multiplier} 
    = w{asteroids = getAsteroids newLists,
        bullets   = getBullets newLists,
        score     = getScore newLists}
            where newLists = doHitting multiplier ([], []) (bullets, asteroids, score)
                  getBullets (b, _, _)   = b
                  getAsteroids (_, a, _) = a
                  getScore (_, _, s) = s

doHitting ::Int -> ([Bullet], [Asteroid]) -> ([Bullet], [Asteroid], Int) -> ([Bullet], [Asteroid], Int)
doHitting _ (clearBullets, clearAsteroids) (bs, [], curScore) = (clearBullets ++ bs, clearAsteroids, curScore)  --we've checked for all asteroids
doHitting mult (clearBullets, clearAsteroids) ([], a:as, curScore) = doHitting mult ([], clearAsteroids ++ [a]) (clearBullets, as, curScore) --this asteroid wasn't hit! phew
doHitting mult (clearBullets, clearAsteroids)  (b:bs, a:as, curScore) 
                | checkBulletCollision b a = doHitting mult ([], []) (clearBullets ++ bs, clearAsteroids ++ splitAsteroid a b ++ as, curScore + mult) -- hit!! check next asteroid, leaving the bullet out and exploding the current asteroid
                | otherwise                = doHitting mult (clearBullets ++ [b], clearAsteroids) (bs, a:as, curScore) -- this bullet didn't hit the asteroid~! check next bullet
                    where splitAsteroid a b = explodeAsteroid a (radToDeg (argV (getPosition a - (getPosition b - getSpeed b))))
-- splits an asteroid in two smaller ones if its size > 1
explodeAsteroid :: Asteroid -> Float -> [Asteroid]
explodeAsteroid Asteroid{positionAs, sizeAs} hitDir | sizeAs == 1 = []  -- destroy small asteroids
                                                    | otherwise   = [newAsteroid positionAs spd s,
                                                                     newAsteroid positionAs (-spd) s]  -- split bigger asteroids into 2 smaller ones
                                                        where s = sizeAs - 1
                                                              spd = toCartesian (1.0 / fromIntegral s, hitDir + 90)

shootUfos :: World -> World
shootUfos w@World{bullets, ufos} = w{ bullets = fst newLists,
                                      ufos = snd newLists } 
                                            where newLists = hitUfos ([], []) (bullets, ufos)

hitUfos :: ([Bullet], [Ufo]) -> ([Bullet], [Ufo]) -> ([Bullet], [Ufo])
hitUfos (clearBullets, clearUfos) (bs, []) = (clearBullets ++ bs, clearUfos) -- all ufos checked
hitUfos (clearBullets, clearUfos) ([], u:us) = hitUfos ([], clearUfos ++ [u]) (clearBullets, us) --this ufo wasn't hit - check for remaining ufos
hitUfos (clearBullets, clearUfos) (b:bs, u:us) | checkUfoHit b u = hitUfos ([], clearUfos ++ [hitUfo u b]) (clearBullets ++ bs, us) --ufo was hit! change affected ufo & continue
                                               | otherwise       = hitUfos (clearBullets ++ [b], clearUfos) (bs, u:us) -- not hit -> continue
                    where hitUfo uf@Ufo{health} bullet = uf{ health = health - 1, 
                                                             speedUfo = getSpeed uf + mulSV 0.2 (getSpeed bullet)}

-- checks if a bullet hits a UFO
checkUfoHit :: Bullet -> Ufo -> Bool
checkUfoHit a b = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

-- checks if there's collision between a Bullet and an Asteroid
checkBulletCollision :: Bullet -> Asteroid -> Bool
checkBulletCollision a b = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

-- checks if a player is hit or killed. If the player is hit but has more than 1 life,
checkKilled :: World -> World
checkKilled w@World{player, asteroids, lives, invincibility, ufos} 
                                        | hit && lives == 1 = explodePlayer 50 w{state = Dead} -- game over !
                                        | hit               = explodePlayer 15 newWorld        -- player loses a life
                                        | otherwise = w                                        -- ...didn't get hit!
                                       where collision = any (==True) (map (checkUfoDeath player) ufos ++
                                                                       map (checkAsteroidDeath player) asteroids) --check if the player hit an asteroid or a UFO
                                             hit = collision && (invincibility == False)
                                             newWorld = w{lives = lives - 1, 
                                                          player = player{invincibilityTimer = 0.0,                 -- set invincibilityTimer to 0
                                                                          speed = - (mulSV 0.5 (getSpeed player))}} -- give the player a speed impulse
                                             explodePlayer n = emitParticles n (getPosition player) 3

-- checks if Player collided with an Asteroid
checkAsteroidDeath :: Player -> Asteroid -> Bool
checkAsteroidDeath a b = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

-- checks if Player collided with a Ufo
checkUfoDeath :: Player -> Ufo -> Bool
checkUfoDeath a b = magV((getPosition b) - (getPosition a)) < (getRadius b + getRadius a)

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

-- ufo smoke particles
ufoSmoke :: [Ufo] -> World -> World
ufoSmoke [] w = w
ufoSmoke (ufo:us) w = ufoSmoke us (smoke ufo)
                where smoke u@Ufo{health} | health <= 0 = emitParticles 1 (getPosition ufo) 1 w
                                          | otherwise = w
    

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

-- keeps track of the duration of the invincibility
setInvincibility :: Player -> World -> World
setInvincibility Player{invincibilityTimer} w | invincibilityTimer < 15.0 = w{invincibility = True}
                                              | otherwise                = w{invincibility = False }

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
startGame w@World{shootAction} | shootAction == Shoot = spawnAsteroids 1 w{ state = Playing, 
                                                                            shootTimer = 0, 
                                                                            player = newPlayer, 
                                                                            asteroids = []}
                               | otherwise = w

-- check if there are any asteroids left. if not: go to the next level!
checkNextLevel :: World -> World
checkNextLevel w@World{asteroids, level, multiplier} 
    | length asteroids == 0 = spawnAsteroids (level+2) w{level = level + 1, invincibility = True, multiplier = multiplier + 1}
    | otherwise = w

-- check highScore
checkHighScore :: World -> World
checkHighScore w@World{score, highScore} = if highScore < score then w{highScore = score} else w

-- converts a vector from a polar coordinate (in degrees) to a cartesian coordinate
toCartesian :: Vector -> Vector
toCartesian polar = rotateV (-degToRad(snd polar)) (fst polar, 0)

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
                        invincibilityTimer :: Float}
-- player init
newPlayer :: Player
newPlayer = Player (0, 0) (0, 0) (-45) 10 0

-- player draw event
instance Drawable Player where
    drawMe p@Player{position, rotation, invincibilityTimer} = drawSprite sprite position 8 rotation
                    where sprite | ((mod (floor invincibilityTimer) 2) == 0) && invincibilityTimer < 15.0 = playerInvincibleSprite
                                 | otherwise = playerSprite

-- player step event
instance Moveable Player where
    getPosition Player{position} = position
    getSpeed Player{speed} = speed
    setPosition p@Player{position} newPos = p{position = newPos}
    getRadius Player{radius} = radius
    getRotation Player{rotation} = rotation
    step World{rotateAction, movementAction, shootAction} p@Player{speed, rotation, invincibilityTimer} = 
        wrap $ addSpeed p 
        { 
            speed = mulSV 0.975 (speed + acceleration movementAction),
            rotation = rotation + rotateDirection rotateAction,
            invincibilityTimer = invincibilityTimer + 0.15
        }
        where   rotateDirection NoRotation = 0
                rotateDirection RotateLeft = -6
                rotateDirection RotateRight = 6
                acceleration NoMovement = (0, 0)
                acceleration Thrust = toCartesian (0.175, rotation)

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
                updateAsteroid i (a@Asteroid{positionAs, speedAs}:as)  | distToPlayer < 100 = updateAsteroid (i+1) (a:as) -- choose other position if too close to the player
                                                                       | otherwise = a{ positionAs = pos, speedAs = spd } : updateAsteroid (i+1) as   -- set new asteroid position and speed, recurse
                                                                            where pos = positions !! i
                                                                                  spd = speeds !! i
                                                                                  distToPlayer = magV (pos - getPosition player)

instance Drawable Asteroid where
    drawMe a@Asteroid{positionAs, rotationAs, spriteAs} = drawSpriteWrapped spriteAs positionAs (getRadius a) rotationAs

instance Moveable Asteroid where
    getPosition Asteroid{positionAs} = positionAs
    setPosition a@Asteroid{positionAs} newPos = a{positionAs = newPos}
    getSpeed a@Asteroid{speedAs} = speedAs
    getRadius Asteroid{sizeAs} = fromIntegral sizeAs * 8.0
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
    step World{player} s@Star{speedSt, z}    = wrap $ addSpeed s{speedSt = mulSV (-1/(z*z)) ((-2, 1))} --getSpeed player + 

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
    drawMe p@Particle{positionPa} = drawSprite starSprite positionPa 1 0

-- UFO DEFINITIONS
data Ufo = Ufo { positionUfo  :: Vector,
                 speedUfo     :: Vector,
                 health       :: Int,
                 repairCounter:: Float}

newUfo :: Vector -> Ufo
newUfo pos = Ufo pos (0, 0) 3 0

instance Moveable Ufo where
    getPosition Ufo{positionUfo} = positionUfo
    setPosition u@Ufo{positionUfo} newPos = u{positionUfo = newPos}
    getSpeed Ufo{speedUfo} = speedUfo
    getRadius _ = 8
    getRotation Ufo{speedUfo, health} = (fst speedUfo) * 17 + (koAngle health)
        where koAngle 0 = 40
              koAngle _ = 0
    step World{player, state} u@Ufo{health, repairCounter} = 
        addSpeed u{ speedUfo = newSpd,
                    health = newHealth,
                    repairCounter = newRepairCounter}
            where toPlayer = normalizeV (getPosition player - getPosition u)
                  toHome = normalizeV ((0, -150) - getPosition u)
                  playingSpeed | health > 0 = mulSV 0.98 ((getSpeed u) + (mulSV 0.017 toPlayer))
                               | otherwise  = mulSV 0.9 (getSpeed u)
                  deadSpeed    | health > 0 = mulSV 0.98 ((getSpeed u) + (mulSV 0.02 toHome))
                               | otherwise  = mulSV 0.9 (getSpeed u)
                  newSpd | state == Playing = playingSpeed
                         | state == Dead    = deadSpeed
                  newRepairCounter | health == 0 = repairCounter + 0.0075
                                   | otherwise   = 0
                  newHealth | health == 0 && repairCounter >= 1 = 3
                            | otherwise = max 0 health

instance Drawable Ufo where
    drawMe u@Ufo{positionUfo} = drawSprite ufoSprite positionUfo 12 (getRotation u)