-- | main.hs
-- | main file for the game

module Main where

import Graphics.Gloss
import System.Random ( randomRIO )
import Control.Monad ( forM, fmap )
import qualified Data.Set as Set

data ViewPort = MkViewPort { pxWidth :: Int, pxHeigth :: Int }
type Coord    = (Float, Float)

data Cell =   Empty
            | Snake
            | Apple
            | Wall
  deriving Show

type Grid = [[Cell]]

gridRows :: Grid -> Int
gridRows = length

gridCols :: Grid -> Int
gridCols = length . head

snakePos :: (Int, Int)
snakePos = (10,10)

gridInitialize :: Int -> Int -> Int -> IO Grid
gridInitialize w h appleCount = do
    apples <- applePositions appleCount (w, h)
    return $ [[ initPos apples (x, y) | x <- [0..w-1]] | y <- [0..h-1]]
  where 
      initPos apples (x, y) =
        if x == 0 || x == w-1 || y == 0 || y == h-1 
          then Wall
        else if (x,y) == snakePos
          then Snake 
        else if Set.member (x,y) apples
          then Apple
        else Empty

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

main :: IO ()
main = do
   grid <- gridInitialize 50 50 5
   display (InWindow "Hello GLOSS" (width, height) (10, 10)) black $ drawGameGrid grid vp
  where (width, height) = (640, 640)
        vp = MkViewPort width height

drawGameGrid :: Grid -> ViewPort -> Picture
drawGameGrid grid vp = pictures cells
    where cells = [ drawCell vp (pos i j) (getCell grid i j) (w, h) | i <- [0..rows-1], j <- [0..cols-1] ]
          pos i j = ((fromIntegral i + 0.5) * w, (fromIntegral j + 0.5) * h)
          rows = gridCols grid
          cols = gridRows grid
          w = 1.0 / fromIntegral rows
          h = 1.0 / fromIntegral cols

gridColor :: Color
gridColor = makeColor8 255 255 255 25

wallColor :: Color
wallColor = makeColor8 128 128 128 250

cellColor :: Cell -> Maybe Color
cellColor Empty = Nothing
cellColor Snake = Just red
cellColor Apple = Just green
cellColor Wall  = Just wallColor