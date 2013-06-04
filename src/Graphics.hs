-- Graphics.hs
-- interfacing with Gloss

module Graphics
        ( ViewPort (..)
        , Coord
        , Cell (..)
        , Grid
        , drawGame
        , loadgameOverBmp
        ) where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import System.Random ( randomRIO )
import Control.Monad ( forM, fmap )

import qualified Data.Set as Set
import qualified Data.List as List

import Game

data ViewPort = MkViewPort { pxWidth :: Int, pxHeigth :: Int }
                deriving (Show, Eq)

type Coord    = (Float, Float)

data Cell 
    = Empty
    | Snake
    | Apple
    | Wall
  deriving Show

type Grid = [[Cell]]

drawGame :: ViewPort -> Picture -> GameState -> Picture
drawGame vp gameOver state = 
        if (isGameOver state)
        then pictures [ grid, gameOver ]
        else grid
     where grid = drawGameGrid vp . gameToGrid $ state     

loadgameOverBmp :: IO Picture
loadgameOverBmp = loadBMP "./GameOver.bmp"   

gameToGrid :: GameState -> Grid
gameToGrid state = [ [getCell(x,y) | x<-[0..w-1]] | y<-[0..h-1]]
  where (w, h) = gridSize state
        getCell pos
          | List.elem pos (snakeBody state)      = Snake
          | Set.member pos (apples state)        = Apple
          | Set.member pos (walls state)         = Wall
          | otherwise                            = Empty

drawGameGrid :: ViewPort -> Grid -> Picture
drawGameGrid vp grid = pictures cells
    where cells = [ drawCell vp (pos i j) (getCell grid i j) (w, h) | i <- [0..rows-1], j <- [0..cols-1] ]
          pos i j = ((fromIntegral i + 0.5) * w, (fromIntegral j + 0.5) * h)
          rows = gridCols grid
          cols = gridRows grid
          w = 1.0 / fromIntegral rows
          h = 1.0 / fromIntegral cols

applePositions :: Int -> (Int, Int) -> IO (Set.Set (Int, Int))
applePositions apples (gw, gh) = insert Set.empty
  where insert m
          | Set.size m >= apples = return m
          | otherwise = do
            m' <- insertRandom m
            insert m'
          where 
            insertRandom m = do
              x <- randomRIO (1, gw-2)
              y <- randomRIO (1, gh-2)
              return $
                if (x,y) == snakePos then m
                else Set.insert (x,y) m

gridRows :: Grid -> Int
gridRows = length

gridCols :: Grid -> Int
gridCols = length . head

getCell :: Grid -> Int -> Int -> Cell
getCell grid x y = grid!!y!!x

pixelWidth :: ViewPort -> Float
pixelWidth = fromIntegral . pxWidth

pixelHeight :: ViewPort -> Float
pixelHeight = fromIntegral . pxHeigth

coordInView :: ViewPort -> Coord -> Coord
coordInView vp (x, y) = (x', y')
    where x' = (x - 0.5) * pixelWidth vp
          y' = (y - 0.5) * pixelHeight vp

sizeInView :: ViewPort -> (Float, Float) -> (Float, Float)
sizeInView vp (w, h) = (w', h')
    where w' = w * pixelWidth vp
          h' = h * pixelHeight vp

scaleSize :: Float -> (Float, Float) -> (Float, Float)
scaleSize f (w, h) = (f*w, f*h)

translateToView :: ViewPort -> Coord -> (Picture -> Picture)
translateToView vp c = translate x (-y)
    where (x, y) = coordInView vp c

drawWire :: ViewPort -> Coord -> Color -> (Float, Float) -> Picture
drawWire vp coord col sz = translateToView vp coord $ rect
    where rect = color col $ rectangleWire w h
          (w, h) = sizeInView vp sz

fillRectangle :: ViewPort -> Coord -> Color -> (Float, Float) -> Picture
fillRectangle vp coord col sz = translateToView vp coord $ rect
    where rect = color col $ rectangleSolid w h
          (w, h) = sizeInView vp sz

drawCell :: ViewPort -> Coord -> Cell -> (Float, Float) -> Picture
drawCell vp coord content sz = 
    case (cellColor content) of
      Just c  -> pictures [cell c, wire]
      Nothing -> wire
    where wire = drawWire vp coord gridColor sz
          cell c = fillRectangle vp coord c sz

gridColor :: Color
gridColor = makeColor8 255 255 255 25

wallColor :: Color
wallColor = makeColor8 128 128 128 250

cellColor :: Cell -> Maybe Color
cellColor Empty = Nothing
cellColor Snake = Just red
cellColor Apple = Just green
cellColor Wall  = Just wallColor