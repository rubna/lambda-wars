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
drawPlaying World{player, asteroids, bullets, stars, particles, ufos, score, lives, multiplier, highScore, level}
    = pictures ([drawMe player] ++ 
                map drawMe asteroids ++
                map drawMe bullets ++
                map drawMe stars ++
                map drawMe particles ++
                map drawMe ufos ++
                [borderPicture] ++
                drawAllText score lives level multiplier highScore ++
                [drawTitle])

-- draw game over screen
drawDead :: World -> Picture
drawDead World{asteroids, bullets, stars, particles, ufos, highScore}
    = pictures (map drawMe asteroids ++
                map drawMe bullets ++ 
                map drawMe stars ++
                map drawMe particles ++
                map drawMe ufos ++
                [borderPicture] ++
                [drawTitle] ++ 
                [drawInstructions]++
                [drawText highScore "highscore: " (-115) (-255)])
-- gameDraw :: [GameObject] -> Picture
-- gameDraw objs = pictures (map drawObject objs)