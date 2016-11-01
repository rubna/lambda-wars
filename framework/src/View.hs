{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution World{player, asteroid}
    = pictures [drawMe player, drawMe asteroid]

-- gameDraw :: [GameObject] -> Picture
-- gameDraw objs = pictures (map drawObject objs)