module Types
  ( State(..)
  , Food(..)
  , Snake(..)
  , KeyPressed(..)
  , Coordinate(..)
  , Bounds(..)
  , Name(..)
  , Tick(..)
  ) where

-- import qualified Snake

data Tick = Tick

data Name = FooBox deriving (Eq, Ord)

-- | Our main game state
data State = State {
    title                :: String,
    keyPressed           :: KeyPressed,
    score                :: Int,
    food                 :: Food,
    snake                :: Snake,
    bounds               :: Bounds
} deriving Show

newtype Snake = Snake
  { getSegments :: [Types.Coordinate]
  } deriving (Show)

-- | Reflect which keys are being pressed
data KeyPressed
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyNone
  deriving (Show)

-- s = Snake { getSegments = [Coordinate{x = 10, y = 100}]}

newtype Food = Food
  { getFood :: [Coordinate]
  } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving Show

data Bounds = Bounds
  { maxWidth :: Int
  , maxHeight :: Int
  } deriving (Show)
