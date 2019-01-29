module State (
             initialState
             )where

import qualified Model as M
import qualified Snake
import qualified System.Random
import qualified Food

initialState :: M.State
initialState =
  M.State
    { M.title = ""
    , M.direction = M.North
    , M.previousDirection = M.North
    , M.score = 0
    , M.food = Food.initialFood
    , M.snake = Snake.initialSnake
    , M.bounds = M.Bounds {M.maxHeight = 20, M.maxWidth = 60}
    , M.randomGenerator = newGenerator
    }

newGenerator :: System.Random.StdGen
newGenerator = System.Random.mkStdGen 42
