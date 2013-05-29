-- | main.hs
-- | main file for the game

module Main where

import Graphics.Gloss

data ViewPort = MkViewPort { pxWidth :: Int, pxHeigth :: Int }
type Coord    = (Float, Float)

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

translateToView :: ViewPort -> Coord -> (Picture -> Picture)
translateToView vp c = translate x (-y)
    where (x, y) = coordInView vp c

drawRectangle :: ViewPort -> Coord -> Color -> (Float, Float) -> Picture
drawRectangle vp coord col sz = translateToView vp coord $ rect
    where rect = color col $ rectangleSolid w h
          (w, h) = sizeInView vp sz

main :: IO ()
main =
	display (InWindow "Hello GLOSS" (width, height) (10, 10)) black $ checkerPicture vp
    where (width, height) = (640, 640)
          vp = MkViewPort width height

checkerPicture :: ViewPort -> Picture
checkerPicture vp = pictures rectangles
    where rectangles = [ drawRectangle vp (x, y) white (w, w) | i <- [0..9], j <- [0..9], let (x, y) = pos i j, (i+j) `mod` 2 == 0 ]
          pos i j = ((fromIntegral i + 0.5) * w, (fromIntegral j + 0.5) * w)
          w = 1.0 / 10.0