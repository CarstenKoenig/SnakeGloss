-- | main.hs
-- | main file for the game

module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import System.Random ( randomRIO )
import Control.Monad ( forM, fmap )

import qualified Data.Set as Set
import qualified Data.List as List

data ViewPort = MkViewPort { pxWidth :: Int, pxHeigth :: Int }
                deriving (Show, Eq)

type GridSize = (Int, Int)
type Coord    = (Float, Float)

data Cell =   Empty
            | Snake
            | Apple
            | Wall
  deriving Show

type Grid = [[Cell]]

type Pos = (Int, Int)

data Snake     = MkSnake { body :: [Pos], isGrowing :: Bool } 
                 deriving (Show, Eq)

type Apples    = Set.Set Pos
type Walls     = Set.Set Pos
type Score     = Int

data Direction 
  = MoveRight 
  | MoveLeft 
  | MoveUp 
  | MoveDown
  deriving (Show, Eq)

data Input
  = KeyDown
  | KeyUp
  | KeyLeft
  | KeyRight
  deriving (Show, Eq)

data GameState 
  = GameState   
    { snake     :: Snake
    , apples    :: Apples
    , walls     :: Walls
    , direction :: Direction
    , score     :: Score
    , gridSize  :: GridSize
    , gameOver  :: Bool
    }
 deriving (Show, Eq)

wrap :: GridSize -> Pos -> Pos
wrap (w, h) (x, y) = (x `mod` w, y `mod` h)

move :: GridSize -> Direction -> Pos -> Pos
move gs MoveRight (x,y) = wrap gs (x+1, y)
move gs MoveLeft  (x,y) = wrap gs (x-1, y)
move gs MoveUp    (x,y) = wrap gs (x, y-1)
move gs MoveDown  (x,y) = wrap gs (x, y+1)

oppositeDirection :: Direction -> Direction
oppositeDirection MoveLeft  = MoveRight
oppositeDirection MoveRight = MoveLeft
oppositeDirection MoveUp    = MoveDown
oppositeDirection MoveDown  = MoveUp

inSameDirection :: Direction -> Input -> Bool
inSameDirection d k = d == inputToDirection k

inOppositeDirection :: Direction -> Input -> Bool
inOppositeDirection d k = inSameDirection (oppositeDirection d) k

inputToDirection :: Input -> Direction
inputToDirection KeyLeft  = MoveLeft
inputToDirection KeyRight = MoveRight
inputToDirection KeyUp    = MoveUp
inputToDirection KeyDown  = MoveDown

directionToInput :: Direction -> Input
directionToInput MoveLeft  = KeyLeft
directionToInput MoveRight = KeyRight
directionToInput MoveUp    = KeyUp
directionToInput MoveDown  = KeyDown

changeDirection :: Direction -> Input -> Direction
changeDirection move key
  | inSameDirection move key || 
    inOppositeDirection move key  = move
  | otherwise                     = inputToDirection key

snakeHead :: Snake -> Pos
snakeHead (MkSnake s _) = head s

removeTail :: Snake -> [Pos]
removeTail (MkSnake s _) = take (length s - 1) $ s

moveSnake :: GridSize -> Direction -> Snake -> Snake
moveSnake gs d s = s { body = newHead s : removeTail s }
  where newHead = move gs d . snakeHead

stepGame :: GameState -> GameState
stepGame state       
  | gameOver state = state
  | otherwise      = state { snake = snake' }
    where snake' = moveSnake (gridSize state) (direction state) (snake state)

initGame :: GridSize -> GameState
initGame gs = GameState snake apples walls MoveRight 0 gs False
  where snake = MkSnake [(2,2)] False
        apples = Set.empty
        walls = Set.empty

gridRows :: Grid -> Int
gridRows = length

gridCols :: Grid -> Int
gridCols = length . head

snakePos :: (Int, Int)
snakePos = (10,10)

gameToGrid :: GameState -> Grid
gameToGrid state = [ [getCell(x,y) | x<-[0..w-1]] | y<-[0..h-1]]
  where (w, h) = gridSize state
        getCell pos
          | List.elem pos (body . snake $ state) = Snake
          | Set.member pos (apples state)        = Apple
          | Set.member pos (walls state)         = Wall
          | otherwise                            = Empty


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

drawGameGrid :: ViewPort -> Grid -> Picture
drawGameGrid vp grid = pictures cells
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