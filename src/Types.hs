module Types
  ( State(..)
  , KeyPressed(..)
  , Coordinate(..)
  , Snake(..)
  , Food(..)
  , Bounds(..)
  , exState
  , exFood
  , exSnake
  , Name(..)
  ) where

import qualified Pointless

data Name = FooBox deriving (Eq, Ord)

-- | Our main game state
data State = State {
    title                :: String,
    keyPressed           :: KeyPressed,
    oscillatingN         :: Int,
    oscillatingDirection :: Pointless.Direction,
    score                :: Int,
    food                 :: Food,
    snake                :: Snake,
    bounds               :: Bounds
} deriving Show

-- | Reflect which keys are being pressed
data KeyPressed
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyNone
  deriving (Show)

newtype Snake = Snake
  { getSegments :: [Coordinate]
  } deriving (Show)

-- s = Snake { getSegments = [Coordinate{x = 10, y = 100}]}

newtype Food = Food
  { getFood :: [Coordinate]
  } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving Show

data Bounds = Bounds
  { maxHeight :: Int
  , maxWidth  :: Int
  } deriving (Show)


-- example snake for mucking around with
exSnake :: Snake
exSnake = Snake { getSegments = [Coordinate{x = 5, y = 5}]}

exFood :: Food
exFood = Food { getFood = [Coordinate{x = 5, y = 6}]}

exState :: State
exState =
  State
    { title = "Hey"
    , keyPressed = KeyUp
    , oscillatingN = 5
    , oscillatingDirection = Pointless.L
    , score = 10
    , food = exFood
    , snake = exSnake
    , bounds = Bounds {maxHeight = 10, maxWidth = 10}
    }
