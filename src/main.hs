-- | main.hs
-- | main file for the game

module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import Game
import Graphics

main :: IO ()
main =
  play  (InWindow "glossSNAKE" (width, height) (10, 10)) 
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
            GlossIO.KeyUp    -> reactGame world KeyUp
            GlossIO.KeyDown  -> reactGame world KeyDown
            GlossIO.KeyLeft  -> reactGame world KeyLeft
            GlossIO.KeyRight -> reactGame world KeyRight
            _   -> world
        react _ world = world     