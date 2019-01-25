module Types
  ( State(..)
  , Food(..)
  , Snake(..)
  , Direction(..)
  , Coordinate(..)
  , Bounds(..)
  , Name(..)
  , Tick(..)
  , KeyPressed(..)
  ) where

-- import qualified Snake

data Tick = Tick

data Name = FooBox deriving (Eq, Ord)

-- | Our main game state
data State = State {
    title             :: String,
    keyPressed        :: KeyPressed,
    direction         :: Direction,
    previousDirection :: Direction,
    score             :: Int,
    food              :: Food,
    snake             :: Snake,
    bounds            :: Bounds
} deriving Show

newtype Snake = Snake
  { getSegments :: [Types.Coordinate]
  } deriving (Show)

data Direction
  = North
  | South
  | West
  | East
  deriving (Show)

data KeyPressed
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyNone
  deriving (Show)

newtype Food = Food
  { getFood :: [Coordinate]
  } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving Show

data Bounds = Bounds
  { maxWidth  :: Int
  , maxHeight :: Int
  } deriving (Show)
