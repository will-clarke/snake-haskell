module State (
             exState
             )where

import qualified Types as T
import qualified Snake
import qualified System.Random
import qualified Food

exState :: T.State
exState =
  T.State
    { T.title = "Some title"
    , T.direction = T.North
    , T.previousDirection = T.North
    , T.keyPressed = T.KeyUp
    , T.score = 0
    , T.food = Food.exFood
    , T.snake = Snake.exSnake
    , T.bounds = T.Bounds {T.maxHeight = 20, T.maxWidth = 60}
    , T.randomGenerator = newGenerator
    }

newGenerator :: System.Random.StdGen
newGenerator = System.Random.mkStdGen 42
