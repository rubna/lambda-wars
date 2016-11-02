{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time w@(World{player, asteroids, bullets}) = 
    shootAsteroids (shoot w{
        player = step w player, 
        asteroids = map (step w) asteroids, 
        bullets = map (step w) bullets
    } player)