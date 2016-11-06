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
playingStep w@(World{state, player, asteroids, bullets, stars, particles, ufos}) = 
    checkKilled
    (setInvincibility player
    (timeOutInstances
    (checkHighScore 
    (checkNextLevel
    (shootAsteroids
    (shootUfos
    (ufoSmoke ufos
    (playerTrail
    (shoot player w{
        player    = step w player, 
        asteroids = map (step w) asteroids, 
        bullets   = map (step w) bullets,
        stars   = map (step w) stars,
        particles   = map (step w) particles,
        ufos   = map (step w) ufos
    })))))))))

deadStep :: World -> World
deadStep w@(World{state, asteroids, bullets, stars, particles, ufos}) = 
    startGame
    (spawnStars (100 - length stars) 
    (timeOutInstances 
    w {
        asteroids = map (step w) asteroids, 
        bullets   = map (step w) bullets,
        stars   = map (step w) stars,
        particles   = map (step w) particles,
        ufos   = map (step w) ufos,
        lives = 3
    }))