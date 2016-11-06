module Sprite where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

-- color definitions

-- sprite definitions
type Sprite = [Point]

drawSprite :: Sprite -> Vector -> Float -> Float -> Picture
drawSprite spr (x, y) scale rot = Translate x y $ Rotate rot $ Color white $ Scale scale scale $ lineLoop spr

drawSpriteWrapped :: Sprite -> Vector -> Float -> Float -> Picture
drawSpriteWrapped spr (x, y) scale rot 
    = pictures [Translate x y sprite,
                Translate (x+w) y sprite,
                Translate (x-w) y sprite,
                Translate x (y-w) sprite,
                Translate x (y+w) sprite]
                    where sprite = Rotate rot $ Color white $ Scale scale scale $ lineLoop spr
                          w = 205 * 2

-- SPRITE INSTANCES
-- player sprite
playerSprite :: Sprite
playerSprite = [(-1, 1), (1, 0), (-1, -1), (-0.5, 0)]

playerInvincibleSprite :: Sprite
playerInvincibleSprite = []

-- asteroid sprite
asteroidSprite :: Int -> Sprite
asteroidSprite vertices = circleVertex (vertices-1) vertices

-- makes a list of vertices in a circle
circleVertex :: Int -> Int -> [Vector]
circleVertex 0 vertices = [(1, 0)]
circleVertex i vertices = rotateV (2 * 3.1415 * (fromIntegral  i) / (fromIntegral vertices)) (1, 0) : circleVertex (i-1) vertices

--ufo sprite
ufoSprite :: Sprite
ufoSprite = [(-1.2, 0), (-0.4, 0.25), (-0.4, 0.5), (-0.2, 0.75), (0.2, 0.75), (0.4, 0.5), (0.35, 0.25),
             (1.2, 0),  (0.7, -0.5), (-0.7, -0.5),
             (-1.2, 0), (-0.4, 0.25), (0.4, 0.25), (1.2, 0)]

-- bullet sprite
bulletSprite :: Sprite
bulletSprite = [(-1, 0), (0.5, -0.25), (0.75, 0), (0.5, 0.25)]

-- star sprite
starSprite :: Sprite
starSprite = circleVertex 5 5

-- border sprite
borderSprite :: Sprite
borderSprite = [(-200.0, -200), (-200, 200), (200, 200), (200, -200)]

borderPicture :: Picture
borderPicture = pictures [Translate (0) (200)  $ Color black $ rectangleUpperSolid 1028 100,   --upper
                          Translate (0) (-300) $ Color black $ rectangleUpperSolid 1028 100,  --lower
                          Translate (-400) (0) $ Color black $ rectangleSolid 400 400,  --left
                          Translate (400) (0)  $ Color black $ rectangleSolid 400 400,  --left
                          drawSprite borderSprite (0, 0) 1 0,       -- inner border
                          drawSprite borderSprite (0, 0) 1.05 0]    -- outer border

-- draw text
drawText :: Int -> String ->  Float -> Float -> Picture
drawText someInt someString x y = Color white $ translate x y $ Scale 0.3 0.3 $ text $ someString ++ show someInt

drawLives :: Int -> Float -> Float -> Picture
drawLives lvs x y = Color white $ translate x y $ Scale 0.3 0.3 $ Rotate (90) $ text (concat $ replicate lvs "A ")

-- draw lives & score
drawAllText :: Int -> Int -> Int ->  Int -> Int -> [Picture]
drawAllText someScore someLives whichLevel whichMultiplier highScore = [drawLives', drawScore, drawLevel, drawMultiplier, drawHighScore]
    where
        drawLives' = drawLives someLives (-255) 170
        drawScore = drawText someScore "Score: " (-450) (190)
        drawLevel = drawText (whichLevel + 1) "Level " 230 30
        drawMultiplier = drawText whichMultiplier "Multiplier: " 230 (-30)
        drawHighScore = drawText highScore "highscore: " (-115) (-255)

-- draw title
drawTitle :: Picture
drawTitle = Translate (-200) 225 $ Scale 0.5 0.5 $  Color white $ text "\\ASTEROIDS"

-- draw "press space to play"
drawInstructions :: Picture
drawInstructions = Translate (-115) (-100) $ Scale 0.3 0.3 $ Color white $ text "press space"