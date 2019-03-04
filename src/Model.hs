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
  , League(..)
  , Leaderboard(..)
  , Score(..)
  , Attempt(..)
  , toTuple
  , toAttempt
  ) where

import qualified Data.Map
import qualified System.Random

data Tick = Tick

-- TOOD: what's this doing again????
data Name = Name deriving (Eq, Ord)

-- | Our main game game
data Game = Game {
    title             :: String,
    snake             :: Snake,
    food              :: Food,
    direction         :: Direction,
    previousDirection :: Direction,
    score             :: Int,
    bounds            :: Bounds,
    graphics          :: Graphics
} deriving Show

data Graphics = Simple | Complex deriving (Show, Read)
-- NB. Read should only really deal with valid Haskell :|
-- instance Read Graphics where
--   readsPrec _ string =
--     case string of
--      "simple" -> return (Simple, [])
--      "complex" -> return (Complex, [])
--      _ -> error $ "Cannot parse " ++ string

data State = StartScreen Options | Playing Game | Paused Game | GameOver Attempt

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
  } deriving (Show, Read, Eq, Ord)


data Attempt = Attempt
  { getLeague :: League
  , getScore :: Score
  } deriving (Show, Eq)

toAttempt :: Game -> Attempt
toAttempt g = Attempt (League (bounds g)) (Score (score g))

toTuple :: Attempt -> (League, Score)
toTuple (Attempt l s) = (l, s)

newtype Score = Score
  { getPoints :: Int
  } deriving (Show, Eq, Ord)

newtype League = League
  { getBounds :: Model.Bounds
  } deriving (Show, Eq, Ord)

newtype Leaderboard = Leaderboard
  { getLeagues :: Data.Map.Map League Score
  } deriving (Show, Eq)
