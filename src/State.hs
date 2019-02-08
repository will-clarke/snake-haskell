module State (
             initialState
             )where

import qualified Food
import qualified Model         as M
import qualified Snake
import qualified System.Random

initialState :: Int -> M.Bounds -> M.State
initialState seed bounds =
  M.State
    { M.title = ""
    , M.direction = M.North
    , M.previousDirection = M.North
    , M.score = 0
    , M.food = food
    , M.snake = Snake.initialSnake
    , M.bounds = bounds
    , M.randomGenerator = nextStdGen
    }
  where
    rng = System.Random.mkStdGen seed
    (food, nextStdGen) = Food.generateRandomFood rng bounds
