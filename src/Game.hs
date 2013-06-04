-- Game.hs
-- the game-models and operations

module Game ( GridSize 
            , Pos
            , Snake
            , Apples
            , Walls
            , Score
            , Direction 
            , Input (..)
            , GameState (..)
            , stepGame
            , reactGame
            , initGame
            , snakeBody 
            , snakePos
            ) where

import qualified Data.Set as Set
import qualified Data.List as List

type GridSize = (Int, Int)
type Pos      = (Int, Int)

data Snake 
    = MkSnake 
        { body :: [Pos]
        , isGrowing :: Bool 
        } deriving (Show, Eq)
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
    } deriving (Show, Eq)

-- exports

stepGame :: GameState -> GameState
stepGame state       
  | gameOver state = state
  | otherwise      = state { snake = snake' }
    where snake' = moveSnake (gridSize state) (direction state) (snake state)

reactGame :: GameState -> Input -> GameState
reactGame state key = state { direction = changeDirection (direction state) key }

initGame :: GridSize -> GameState
initGame gs = GameState snake apples walls MoveRight 0 gs False
  where snake = MkSnake [(2,2)] False
        apples = Set.empty
        walls = Set.empty

snakePos :: (Int, Int)
snakePos = (10,10)

snakeBody :: GameState -> [Pos]
snakeBody = body . snake

-- helpers

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