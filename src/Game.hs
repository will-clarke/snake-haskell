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
    { M.getTitle = ""
    , M.direction = M.East
    , M.previousDirection = M.East
    , M.getScore = 0
    , M.getFood = food
    , M.getSnake = Snake.initialSnake bounds
    , M.getBounds = bounds
    , M.getGraphics = graphics
    }
  where
    rng = System.Random.mkStdGen seed
    food = Food.generateRandomFood rng bounds
