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

import System.Random ( randomRIO )
import Control.Monad ( fmap )

type GridSize = (Int, Int)
type Pos      = (Int, Int)

data Snake 
    = Snake 
        { body          :: [Pos]
        , isGrowing     :: Bool
        , moveDirection :: Direction 
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
    , isGameOver  :: Bool
    } deriving (Show, Eq)

-- exports

stepGame :: GameState -> GameState
stepGame state       
  | isGameOver state' = state'
  | otherwise         = moveStep . eatStep $ state'
  where state' = checkGameOver state

reactGame :: GameState -> Input -> GameState
reactGame state key = state { direction = changeDirection (snakeDirection $ state) key }

initGame :: GridSize -> Int -> IO GameState
initGame gs aplCnt = do
  apples <- fmap Set.fromList $ randomApples aplCnt occupied
  return $ GameState snake apples walls MoveRight 0 gs False
  where walls = Set.empty
        snakeParts = [(2,2), (2,3)]
        snake = createSnake snakeParts False MoveRight
        occupied = Set.union (Set.fromList snakeParts) walls
        (gw, gh) = gs
        randomApples cnt noGo =
          if cnt <= 0 
            then return [] 
            else do
              x <- randomRIO (0, gw-1)
              y <- randomRIO (0, gh-1)
              if Set.member (x,y) noGo
                then randomApples cnt noGo
                else fmap ((x,y) :) (randomApples (cnt-1) (Set.insert (x,y) noGo))


snakePos :: (Int, Int)
snakePos = (10,10)

-- helpers

moveStep :: GameState -> GameState
moveStep state = state { snake = moveSnake state }

moveSnake :: GameState -> Snake
moveSnake state = (snake state) { body = newHead : newTail, moveDirection = direction state, isGrowing = False }
  where newHead = nextPos state
        newTail = if snakeGrowing state then snakeBody state else removeTail state

nextPos :: GameState -> Pos
nextPos state = move (gridSize state) (direction $ state) (snakeHead $ state)

eatStep :: GameState -> GameState
eatStep state = if Set.member next apls
                then state { snake = grow s, apples = eat apls next }
                else state
                where s     = snake state 
                      next  = nextPos state
                      apls  = apples state

grow :: Snake -> Snake
grow s = s { isGrowing = True }

eat :: Apples -> Pos -> Apples
eat apples pos = Set.delete pos apples

checkGameOver :: GameState -> GameState
checkGameOver state = if willBiteItself state || willBiteWall state
                      then gameOver state
                      else state

gameOver :: GameState -> GameState
gameOver state = state { isGameOver = True }

willBiteItself :: GameState -> Bool
willBiteItself state = List.elem (nextPos state) (snakeBody $ state)

willBiteWall :: GameState -> Bool
willBiteWall state = Set.member (nextPos state) (walls state)

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

createSnake :: [Pos] -> Bool -> Direction -> Snake
createSnake = Snake

snakeBody :: GameState -> [Pos]
snakeBody = body . snake

snakeHead :: GameState -> Pos
snakeHead = head . snakeBody

snakeTail :: GameState -> [Pos]
snakeTail = tail . snakeBody

snakeDirection :: GameState -> Direction
snakeDirection = moveDirection . snake

snakeGrowing :: GameState -> Bool
snakeGrowing = isGrowing . snake

removeTail :: GameState -> [Pos]
removeTail = removeEnd . snakeBody
  where removeEnd ls = take (length ls - 1) ls