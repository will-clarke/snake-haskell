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

import qualified Control.Concurrent.STM
import qualified Data.Map
import qualified System.Random

data Tick = Tick

-- TOOD: what's this doing again????
data Name = Name deriving (Eq, Ord)

-- | Our main game game
data Game = Game {
    getTitle          :: String,
    getSnake          :: Snake,
    getFood           :: Food,
    direction         :: Direction,
    previousDirection :: Direction,
    getScore          :: Int,
    getBounds         :: Bounds,
    getGraphics       :: Graphics,
    getSpeedControl   :: Control.Concurrent.STM.TVar Int,
    getPreviousGames  :: [Game]
}

data Graphics = Simple | Complex deriving (Show, Read)

data State
  = StartScreen Options
                (Control.Concurrent.STM.TVar Int)
  | Playing Game
  | Paused Game
  | Replaying Game
  | GameOver Game

data Options = Options
  { getStartSeed     :: Int
  , getStartGraphics :: Model.Graphics
  , getStartHeight   :: Int
  , getStartWidth    :: Int
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
  { getCoordinates :: [Coordinate],
    getRNG         :: System.Random.StdGen
  } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Eq, Show)

data Bounds = Bounds
  { getMaxWidth  :: Int
  , getMaxHeight :: Int
  } deriving (Show, Read, Eq, Ord)


data Attempt = Attempt
  { getAttemptLeague :: League
  , getAttemptScore  :: Score
  } deriving (Show, Eq)

toAttempt :: Game -> Attempt
toAttempt g = Attempt (League (getBounds g)) (Score (getScore g))

toTuple :: Attempt -> (League, Score)
toTuple (Attempt l s) = (l, s)

newtype Score = Score
  { getPoints :: Int
  } deriving (Show, Eq, Ord)

newtype League = League
  { getLeagueBounds :: Model.Bounds
  } deriving (Show, Eq, Ord)

newtype Leaderboard = Leaderboard
  { getLeaguesAndScores :: Data.Map.Map League Score
  } deriving (Show, Eq)
