module Sprite where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

-- sprite definitions
type Sprite = [Point]

drawSprite :: Sprite -> Vector -> Float -> Picture
drawSprite spr (x, y) rot = Translate x y (Rotate rot (Color (makeColor 1 1 1 1) (Scale 10 10 (Line spr))))

-- sprite instances
playerSprite :: Sprite
playerSprite = [(-1, 1), (1, 0), (-1, -1), (-0.5, 0), (-1, 1)]
