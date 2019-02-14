module Game (
             initialGame
             )where

import qualified Food
import qualified Model         as M
import qualified Snake
import qualified System.Random

initialGame :: Int -> M.Bounds -> M.Graphics -> M.Game
initialGame seed bounds graphics =
  M.Game
    { M.title = ""
    , M.direction = M.East
    , M.previousDirection = M.East
    , M.score = 0
    , M.food = food
    , M.snake = Snake.initialSnake bounds
    , M.bounds = bounds
    , M.graphics = graphics
    }
  where
    rng = System.Random.mkStdGen seed
    food = Food.generateRandomFood rng bounds
