module Sprite where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

-- color definitions

-- sprite definitions
type Sprite = [Point]

drawSprite :: Sprite -> Vector -> Float -> Float -> Picture
drawSprite spr (x, y) scale rot = Translate x y (Rotate rot (Color white (Scale scale scale (lineLoop spr))))

-- SPRITE INSTANCES
-- player sprite
playerSprite :: Sprite
playerSprite = [(-1, 1), (1, 0), (-1, -1), (-0.5, 0)]

-- asteroid sprite
asteroidSprite :: Int -> Sprite
asteroidSprite vertices = circleVertex (vertices-1) vertices

-- makes a list of vertices in a circle
circleVertex :: Int -> Int -> [Vector]
circleVertex 0 vertices = [(1, 0)]
circleVertex i vertices = rotateV (2 * 3.1415 * (fromIntegral  i) / (fromIntegral vertices)) (1, 0) : circleVertex (i-1) vertices

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
borderPicture = pictures [Translate (0) (200) (Color black (rectangleUpperSolid 1028 100)),   --upper
                          Translate (0) (-300) (Color black (rectangleUpperSolid 1028 100)),  --lower
                          Translate (-300) (0) (Color black (rectangleSolid 200 400)),  --left
                          Translate (300) (0) (Color black (rectangleSolid 200 400)),  --left
                          drawSprite borderSprite (0, 0) 1 0,       -- inner border
                          drawSprite borderSprite (0, 0) 1.05 0]    -- outer border