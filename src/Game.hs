module Game (
             initialGame
             )where

import qualified Food
import qualified Model         as M
import qualified Snake
import qualified System.Random

initialGame :: M.Options -> M.Game
initialGame (M.Options seed graphics width height) =
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
    bounds = M.Bounds width height
    rng = System.Random.mkStdGen seed
    food = Food.generateRandomFood rng bounds
