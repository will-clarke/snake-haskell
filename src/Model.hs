module Model
  ( State(..)
  , Food(..)
  , Snake(..)
  , Direction(..)
  , Coordinate(..)
  , Bounds(..)
  , Name(..)
  , Tick(..)
  , Game(..)
  , Graphics(..)
  ) where

import qualified System.Random

data Tick = Tick

data Name = FooBox deriving (Eq, Ord)

-- | Our main game state
data State = State {
    title             :: String,
    snake             :: Snake,
    food              :: Food,
    direction         :: Direction,
    previousDirection :: Direction,
    score             :: Int,
    bounds            :: Bounds,
    graphics :: Graphics
} deriving Show

data Graphics = Simple | Complex deriving (Show)

data Game = StartScreen | Playing State | GameOver Int

newtype Snake = Snake
  { getSegments :: [Model.Coordinate]
  } deriving (Show)

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

data Food = Food
  { getFood :: [Coordinate],
    getRNG :: System.Random.StdGen
  } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Eq, Show)

data Bounds = Bounds
  { maxWidth  :: Int
  , maxHeight :: Int
  } deriving (Show)
