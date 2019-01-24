module State (
             exState
             )where

import qualified Types as T
import qualified Snake
import qualified Food

exState :: T.State
exState =
  T.State
    { T.title = "Hey"
    , T.keyPressed = T.KeyUp
    , T.score = 10
    , T.food = Food.exFood
    , T.snake = Snake.exSnake
    , T.bounds = T.Bounds {T.maxHeight = 20, T.maxWidth = 60}
    }
