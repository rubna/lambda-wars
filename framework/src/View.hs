{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Sprite
import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution w@World{state}
    | state == Playing = drawPlaying w
    | otherwise        = drawDead w

-- draw while playing
drawPlaying :: World -> Picture
drawPlaying World{player, asteroids, bullets, stars}
    = pictures ([drawMe player] ++ 
                map drawMe asteroids ++
                map drawMe bullets ++
                map drawMe stars ++
                [borderPicture])

-- draw game over screen
drawDead :: World -> Picture
drawDead World{asteroids, bullets, stars}
    = pictures (map drawMe asteroids ++
                map drawMe bullets ++ 
                map drawMe stars ++
                [borderPicture])
-- gameDraw :: [GameObject] -> Picture
-- gameDraw objs = pictures (map drawObject objs)