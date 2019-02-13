module Model
  ( Game(..)
  , Food(..)
  , Snake(..)
  , Direction(..)
  , Coordinate(..)
  , Bounds(..)
  , Name(..)
  , Tick(..)
  , State(..)
  , Graphics(..)
  , Options(..)
  ) where

import qualified System.Random

data Tick = Tick

-- TOOD: what's this doing again????
data Name = FooBox deriving (Eq, Ord)

-- | Our main game game
data Game = Game {
    title             :: String,
    snake             :: Snake,
    food              :: Food,
    direction         :: Direction,
    previousDirection :: Direction,
    score             :: Int,
    bounds            :: Bounds,
    graphics          :: Graphics,
    alive             :: Bool
} deriving Show

data Graphics = Simple | Complex deriving (Show, Read)
-- NB. Read should only really deal with valid Haskell :|
-- instance Read Graphics where
--   readsPrec _ string =
--     case string of
--      "simple" -> return (Simple, [])
--      "complex" -> return (Complex, [])
--      _ -> error $ "Cannot parse " ++ string

data State = StartScreen Options | Playing Game | GameOver Int

data Options = Options
  { startSeed     :: Int
  , startBounds   :: Model.Bounds
  , startGraphics :: Model.Graphics
  } deriving (Show)


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
    getRNG  :: System.Random.StdGen
  } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Eq, Show)

data Bounds = Bounds
  { maxWidth  :: Int
  , maxHeight :: Int
  } deriving (Show, Read)
