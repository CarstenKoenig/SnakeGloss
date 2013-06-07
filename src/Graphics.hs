{- |
Module      :  Graphics.hs
Description :  draws the game world using GLOSS
Copyright   :  (c) Carsten König
License     :  empty

Maintainer  :  Carsten@gettingsharper.de
Stability   :  experimental
Portability :  portable

here all declerations and functions for the basic drawing operations are collected

it will draw the world using a grid and showing

  * a wall as a grey square

  * the snake as a red chain of squares
  
  * an apple as a green square

-}

module Graphics (
        -- * Types
          ViewPort
        , Coord
        , Cell
        , Grid
        -- * Functions
        , loadgameOverBmp
        , createViewPort
        , drawGame
        ) where

import Graphics.Gloss

import qualified Data.Set as Set
import qualified Data.List as List

import Game

-- | the viewport is defined using the pixel-width and pixel-height
-- of the window drawn to, it is used to calculate relative positions
data ViewPort = 
    MkViewPort 
    { pxWidth :: Int
    , pxHeigth :: Int 
    } deriving (Show, Eq)

-- | the (relative) coordinates of a point in the viewport
-- both x, and y components range between 0 and 1
type Coord    = (Float, Float)

-- | the State of a cell (a square) in the game
data Cell 
    = Empty -- ^the cell is empty
    | Snake -- ^the cell is part of the snake
    | Apple -- ^the cell is occupied by an apple
    | Wall  -- ^the cell is part of a wall
  deriving Show

-- | the grid is just a list of rows (each row being a list of Cells)
type Grid = [[Cell]]

-- | creates a viewport for the given resolution
createViewPort :: (Int, Int) -> ViewPort
createViewPort (w, h) = MkViewPort w h

-- | draws the current game state into a 'Picture'
drawGame :: ViewPort  -- ^the viewport of used to calculate the coordinates
         -> Picture   -- ^a picture that is drawn on top of the world if the game is over
         -> GameState -- ^the current state of the game
         -> Picture   -- ^a picture showing the current gamestate
drawGame vp gameOver state = 
        if (isGameOver state)
        then pictures [ grid, gameOver ]
        else grid
     where grid = drawGameGrid vp . gameToGrid $ state     

-- | loads the gameover-picture (from a file named GameOver.bmp - must be an 24 or 32 bit bitmap without compression)
loadgameOverBmp :: IO Picture
loadgameOverBmp = loadBMP "./GameOver.bmp"   

-- | transforms a 'GameState' into a 'Grid' description of it
gameToGrid :: GameState -> Grid
gameToGrid state = [ [ getContent(x,y) | x<-[0..w-1]] | y<-[0..h-1]]
  where (w, h) = gridSize state
        getContent pos
          | List.elem pos (snakeBody state)      = Snake
          | Set.member pos (apples state)        = Apple
          | Set.member pos (walls state)         = Wall
          | otherwise                            = Empty

-- | draws a grid into a 'Picture'
drawGameGrid :: ViewPort -> Grid -> Picture
drawGameGrid vp grid = pictures cells
    where cells = [ drawCell vp (pos i j) (getCell grid i j) (w, h) | i <- [0..rows-1], j <- [0..cols-1] ]
          pos i j = ((fromIntegral i + 0.5) * w, (fromIntegral j + 0.5) * h)
          rows = gridCols grid
          cols = gridRows grid
          w = 1.0 / fromIntegral rows
          h = 1.0 / fromIntegral cols

-- | the number of rows
gridRows :: Grid -> Int
gridRows = length

-- | the number of columns
gridCols :: Grid -> Int
gridCols = length . head

-- | returns the content on a given position (row -> col)
getCell :: Grid -> Int -> Int -> Cell
getCell grid x y = grid!!y!!x

-- | the pixel-width
pixelWidth :: ViewPort -> Float
pixelWidth = fromIntegral . pxWidth

-- | the pixel-height
pixelHeight :: ViewPort -> Float
pixelHeight = fromIntegral . pxHeigth

-- | transforms the relative coordinates into screen coordinates
coordInView :: ViewPort -> Coord -> Coord
coordInView vp (x, y) = (x', y')
    where x' = (x - 0.5) * pixelWidth vp
          y' = (y - 0.5) * pixelHeight vp

-- | transforms a relative size into screen size
sizeInView :: ViewPort -> (Float, Float) -> (Float, Float)
sizeInView vp (w, h) = (w', h')
    where w' = w * pixelWidth vp
          h' = h * pixelHeight vp

-- | yields a 'Picture' transformation that translates the picture to a relative coordinate
translateToView :: ViewPort -> Coord -> (Picture -> Picture)
translateToView vp c = translate x (-y)
    where (x, y) = coordInView vp c

-- | draws a wire-rectangle in the given color with the given size into a picture
drawWire :: ViewPort -> Coord -> Color -> (Float, Float) -> Picture
drawWire vp coord col sz = translateToView vp coord $ rect
    where rect = color col $ rectangleWire w h
          (w, h) = sizeInView vp sz

-- | draws a filled-rectangle in the given color with the given size into a picture
fillRectangle :: ViewPort -> Coord -> Color -> (Float, Float) -> Picture
fillRectangle vp coord col sz = translateToView vp coord $ rect
    where rect = color col $ rectangleSolid w h
          (w, h) = sizeInView vp sz

-- | draws a cell with the given size and coordinate into a picture
drawCell :: ViewPort -> Coord -> Cell -> (Float, Float) -> Picture
drawCell vp coord content sz = 
    case (cellColor content) of
      Just c  -> pictures [cell c, wire]
      Nothing -> wire
    where wire = drawWire vp coord gridColor sz
          cell c = fillRectangle vp coord c sz

-- | the grids-color
gridColor :: Color
gridColor = makeColor8 255 255 255 25

-- | the wall color
wallColor :: Color
wallColor = makeColor8 128 128 128 250

-- | translates 'Cell'-contents into colors
cellColor :: Cell -> Maybe Color
cellColor Empty = Nothing
cellColor Snake = Just red
cellColor Apple = Just green
cellColor Wall  = Just wallColor