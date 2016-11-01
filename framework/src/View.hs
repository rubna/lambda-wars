{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution World{player, asteroids, bullets}
    = pictures ([drawMe player] ++ 
                map drawMe asteroids ++
                map drawMe bullets)

-- gameDraw :: [GameObject] -> Picture
-- gameDraw objs = pictures (map drawObject objs)