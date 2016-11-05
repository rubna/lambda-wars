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
timeHandler time w@World{state}
    | state == Playing = playingStep w
    | state == Dead    = deadStep w
      
playingStep :: World -> World
playingStep w@(World{state, player, asteroids, bullets, stars, particles}) = 
    checkKilled
    (timeOutInstances
    (spawnStars (100 - length stars) 
    (spawnAsteroids (7 - length asteroids) 
    (shootAsteroids
    (playerTrail
    (shoot player w{
        player    = step w player, 
        asteroids = map (step w) asteroids, 
        bullets   = map (step w) bullets,
        stars   = map (step w) stars,
        particles   = map (step w) particles
    }))))))

deadStep :: World -> World
deadStep w@(World{state, asteroids, bullets, stars, particles}) = 
      startGame
      (timeOutInstances 
      (spawnAsteroids (7 - length asteroids)
      w {
            asteroids = map (step w) asteroids, 
            bullets   = map (step w) bullets,
            stars   = map (step w) stars,
        particles   = map (step w) particles
      }))