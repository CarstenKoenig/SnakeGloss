{- |
Module      :  main.hs
Description :  initialises and displays the game window
Copyright   :  (c) Carsten König
License     :  empty

Maintainer  :  Carsten@gettingsharper.de
Stability   :  experimental
Portability :  portable

main module for the snake game

using the Gloss library this game let's you controll a snake with the arrow-keys:

  * if you eat a green apple your snake will grow

  * if you run into a wall or into yourself your snake will die and the game will be over

-}


module Main 
  ( -- * Functions
    main
  ) where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import Game
import Graphics

-- | the game-windows size
windowSize :: (Int, Int)
windowSize = (640, 480)

-- | viewport based on the windowsize
viewPort :: ViewPort
viewPort = createViewPort windowSize

-- | main entry point to the game
-- loads the needed bitmaps, initializes the game
-- and starts it
main :: IO ()
main = do
  gameOverBmp <- loadgameOverBmp
  initialGame <- initGame (50, 50) 20
  play  (InWindow "glossSNAKE" windowSize (10, 10)) 
        black -- background color
        60 -- sixty frames per second (speed)
        initialGame
        (drawGame viewPort gameOverBmp)
        react
        runGame
  where react (GlossIO.EventKey (GlossIO.SpecialKey k) GlossIO.Down _ _) world =
          case k of
            GlossIO.KeyUp    -> reactGame world KeyUp
            GlossIO.KeyDown  -> reactGame world KeyDown
            GlossIO.KeyLeft  -> reactGame world KeyLeft
            GlossIO.KeyRight -> reactGame world KeyRight
            _   -> world
        react _ world = world     