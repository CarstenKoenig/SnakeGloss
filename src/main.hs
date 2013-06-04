-- | main.hs
-- | main file for the game

module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import System.Random ( randomRIO )
import Control.Monad ( forM, fmap )

import qualified Data.Set as Set
import qualified Data.List as List

import Game
import Graphics

main :: IO ()
main =
  play  (InWindow "snkaeGLOSS" (width, height) (10, 10)) 
        black -- background color
        2 -- two step per second
        initialGame
        (drawGameGrid vp . gameToGrid)
        react
        (\ _ game -> stepGame game)
  where (width, height) = (640, 640)
        vp = MkViewPort width height
        initialGame = initGame (50, 50)
        react (GlossIO.EventKey (GlossIO.SpecialKey k) GlossIO.Down _ _) world =
          case k of
            GlossIO.KeyUp    -> world { direction = changeDirection (direction world) KeyUp }
            GlossIO.KeyDown  -> world { direction = changeDirection (direction world) KeyDown }
            GlossIO.KeyLeft  -> world { direction = changeDirection (direction world) KeyLeft }
            GlossIO.KeyRight -> world { direction = changeDirection (direction world) KeyRight }
            _   -> world
        react _ world = world     