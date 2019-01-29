module State (
             exState
             )where

import qualified Model as M
import qualified Snake
import qualified System.Random
import qualified Food

exState :: M.State
exState =
  M.State
    { M.title = "Some title"
    , M.direction = M.North
    , M.previousDirection = M.North
    , M.score = 0
    , M.food = Food.exFood
    , M.snake = Snake.exSnake
    , M.bounds = M.Bounds {M.maxHeight = 20, M.maxWidth = 60}
    , M.randomGenerator = newGenerator
    }

newGenerator :: System.Random.StdGen
newGenerator = System.Random.mkStdGen 42
