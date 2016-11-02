module Sprite where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

-- sprite definitions
type Sprite = [Point]

drawSprite :: Sprite -> Vector -> Float -> Float -> Picture
drawSprite spr (x, y) scale rot = Translate x y (Rotate rot (Color (makeColor 1 1 1 1) (Scale scale scale (Line spr))))

-- sprite instances
playerSprite :: Sprite
playerSprite = [(-1, 1), (1, 0), (-1, -1), (-0.5, 0), (-1, 1)]

-- asteroid sprite
asteroidSprite :: Int -> Sprite
asteroidSprite vertices = asteroidVertex vertices vertices

-- makes a list of vertices in a circle
asteroidVertex :: Int -> Int -> [Vector]
asteroidVertex 0 vertices = [(1, 0)]
asteroidVertex i vertices = rotateV (2 * 3.1415 * (fromIntegral  i) / (fromIntegral vertices)) (1, 0) : asteroidVertex (i-1) vertices

-- bullet sprite
bulletSprite :: Sprite
bulletSprite = [(-1, 0), (0.5, -0.25), (0.75, 0), (0.5, 0.25), (-1, 0)]