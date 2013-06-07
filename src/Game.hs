{- |
Module      :  Game.hs
Description :  the basic game logic for snakes
Copyright   :  (c) Carsten König
License     :  empty

Maintainer  :  Carsten@gettingsharper.de
Stability   :  experimental
Portability :  portable

here all declerations and functions for the basic game logic are collected

the current game-state with everything relevant is stored in a 'GameState'-record
-}

module Game ( 
  -- * Types
    GridSize 
  , Pos
  , Snake
  , Apples
  , Walls
  , Score
  , Direction 
  , Input (..)
  , GameState (..)
  -- * Functions
  -- ** Initialization
  , initGame
  -- ** Controling the game state
  , runGame
  , reactGame
  -- ** Helpers
  , snakeBody 
  ) where

import qualified Data.Set as Set
import qualified Data.List as List

import System.Random ( randomRIO )

-- | the size of the grid ... here the snake may roam
type GridSize = (Int, Int)

-- | a position in the grid
type Pos      = (Int, Int)

-- | the snakes state
-- the snake is managed as a list of positions
-- the head of this list is the position of the snakes head
-- in addition there are flags indicating that the snake will grow
-- and the direction the snake is moving to
data Snake 
    = Snake 
        { body          :: [Pos]
        , isGrowing     :: Bool   
        , moveDirection :: Direction
        } deriving (Show, Eq)

-- | the position of apples in the grid
type Apples    = Set.Set Pos

-- | the position of walls in the grid
type Walls     = Set.Set Pos

-- | game score
type Score     = Int

-- | the direction the snake will move to
data Direction 
  = MoveRight 
  | MoveLeft 
  | MoveUp 
  | MoveDown
  deriving (Show, Eq)

-- | user input
data Input
  = KeyDown
  | KeyUp
  | KeyLeft
  | KeyRight
  deriving (Show, Eq)

-- | the game-state
data GameState 
  = GameState   
    { snake       :: Snake      -- ^information on the snake
    , apples      :: Apples     -- ^where are the apples
    , walls       :: Walls      -- ^where are the walls
    , direction   :: Direction  -- ^where should the snake move to next (input buffer)
    , score       :: Score      -- ^the current score
    , gridSize    :: GridSize   -- ^the games grid-size
    , isGameOver  :: Bool       -- ^is the game over
    , speed       :: Float      -- ^after this many secods the snake should move
    , moveInSecs  :: Float      -- ^when should the snake move again?
    } deriving (Show, Eq)

-- | checks if the enough time has passed to make a step and does so if yes
runGame :: Float -> GameState -> GameState
runGame secPassed state
  | timeOverStep <= 0 = runGame 0 $ state'
  | otherwise         = state'
  where state'        = (stepGame state) { moveInSecs = speed state + timeOverStep }
        timeOverStep  = moveInSecs state - secPassed


-- | updates the game-state:
-- * if the snake will bite itself or a wall the game will end
-- * the game will eat apples and grow
-- * the snake will move
stepGame :: GameState -> GameState
stepGame state      
  | isGameOver state' = state'
  | otherwise         = moveStep . eatStep $ state'
  where state' = checkGameOver state

-- | reacts to user input by checking if the direction change is possible and setting it for the next action
reactGame :: GameState -> Input -> GameState
reactGame state key = state { direction = changeDirection (snakeDirection $ state) key }

-- | initializes a gamestate with the given grid-size and the given number of apples to randomly place on the grid
initGame :: GridSize -> Int -> IO GameState
initGame gs aplCnt = do
  apls <- fmap Set.fromList $ randomApples aplCnt occupied
  return $ GameState sk apls wls MoveRight 0 gs False 0.5 0.5
  where wls = Set.empty
        snakeParts = [(2,2), (2,3)]
        sk = createSnake snakeParts False MoveRight
        occupied = Set.union (Set.fromList snakeParts) wls
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

-- | moves the snake along
moveStep :: GameState -> GameState
moveStep state = state { snake = moveSnake state }

-- | checks if the snake will eat an apple, and if so
-- sets the isGrowing flag and removes the apple from the game
eatStep :: GameState -> GameState
eatStep state = if Set.member next apls
                then state { snake = grow s, apples = eat apls next, score = score state + 1 }
                else state
                where s     = snake state 
                      next  = nextPos state
                      apls  = apples state

-- | checks if the game will end because the snake will bite itself or a wall
checkGameOver :: GameState -> GameState
checkGameOver state = if willBiteItself state || willBiteWall state
                      then gameOver state
                      else state

-- | our world is a donat-world so if the snake leaves right
-- it will return left, etc.
wrap :: GridSize -> Pos -> Pos
wrap (w, h) (x, y) = (x `mod` w, y `mod` h)

-- | moves a position along the given direction wraping around the donut-world
move :: GridSize -> Direction -> Pos -> Pos
move gs MoveRight (x,y) = wrap gs (x+1, y)
move gs MoveLeft  (x,y) = wrap gs (x-1, y)
move gs MoveUp    (x,y) = wrap gs (x, y-1)
move gs MoveDown  (x,y) = wrap gs (x, y+1)

-- | reverts the direction
oppositeDirection :: Direction -> Direction
oppositeDirection MoveLeft  = MoveRight
oppositeDirection MoveRight = MoveLeft
oppositeDirection MoveUp    = MoveDown
oppositeDirection MoveDown  = MoveUp

-- | is the input in the same direction?
inSameDirection :: Direction -> Input -> Bool
inSameDirection d k = d == inputToDirection k

-- | is the input in the opposite direction?
inOppositeDirection :: Direction -> Input -> Bool
inOppositeDirection d k = inSameDirection (oppositeDirection d) k

-- | input to direction
inputToDirection :: Input -> Direction
inputToDirection KeyLeft  = MoveLeft
inputToDirection KeyRight = MoveRight
inputToDirection KeyUp    = MoveUp
inputToDirection KeyDown  = MoveDown

-- | if possible changes the direction, if not stays the same
  -- it is possible to change the direction if it is not the same or the opposite (so the snake will move into itself)
changeDirection :: Direction -> Input -> Direction
changeDirection m key
  | inSameDirection m key || 
    inOppositeDirection m key  = m
  | otherwise                     = inputToDirection key

-- | uses the next user-direction to return the next position of the snakes head
nextPos :: GameState -> Pos
nextPos state = move (gridSize state) (direction $ state) (snakeHead $ state)

-- | creates a snake record
createSnake :: [Pos] -> Bool -> Direction -> Snake
createSnake = Snake

-- | move the snake along it's path
moveSnake :: GameState -> Snake
moveSnake state = (snake state) { body = newHead : newTail, moveDirection = direction state, isGrowing = False }
  where newHead = nextPos state
        newTail = if snakeGrowing state then snakeBody state else removeTail state

-- | yields the cell-positions the snake occupies
snakeBody :: GameState -> [Pos]
snakeBody = body . snake

-- | yields the cell-position of the snakes head
snakeHead :: GameState -> Pos
snakeHead = head . snakeBody

-- | yields the direction the snake is currently moving in
snakeDirection :: GameState -> Direction
snakeDirection = moveDirection . snake

-- | yields if the snake has eaten an apple and needs to grow
snakeGrowing :: GameState -> Bool
snakeGrowing = isGrowing . snake

-- | removes the last part of the snake (in order to move it along)
removeTail :: GameState -> [Pos]
removeTail = removeEnd . snakeBody
  where removeEnd ls = take (length ls - 1) ls

-- | has the game already ended?
gameOver :: GameState -> GameState
gameOver state = state { isGameOver = True }

-- | is the snake going to bite itself?
willBiteItself :: GameState -> Bool
willBiteItself state = List.elem (nextPos state) (snakeBody $ state)

-- | is the snake going to bite a wall?
willBiteWall :: GameState -> Bool
willBiteWall state = Set.member (nextPos state) (walls state)

-- | sets the isGrowing flag on the snake
grow :: Snake -> Snake
grow s = s { isGrowing = True }

-- | removes an eaten apple from the apple-positions
eat :: Apples -> Pos -> Apples
eat apls pos = Set.delete pos apls