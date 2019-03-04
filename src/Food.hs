module Food
  ( generateRandomFood
  , snakeEating
  , calculateScore
  , update
  ) where

import qualified Model       as M
import qualified System.Random

calculateScore :: Int -> M.Snake -> M.Food -> Int
calculateScore oldScore snake food =
  if eating
    then oldScore + 1
    else oldScore
  where
    eating = snakeEating snake food

isFoodAt :: M.Coordinate -> M.Food -> Bool
isFoodAt coord food = elem coord $ M.getCoordinates food

snakeEating :: M.Snake -> M.Food -> Bool
snakeEating s = isFoodAt (head $ M.getSegments s)

-- What happens when we tick food...
update :: M.Game -> M.Food
update game =
  let currentFood = M.getFood game
      snake = M.getSnake game
      bounds = M.getBounds game
      eating = snakeEating snake currentFood
      food = M.getFood game
      foodRNG = M.getRNG food
   in if eating
        then generateRandomFood foodRNG bounds
        else food

generateRandomFood :: System.Random.StdGen -> M.Bounds -> M.Food
generateRandomFood rng bounds =
  let (randX, rng2) = System.Random.random rng :: (Int, System.Random.StdGen)
      (randY, rng3) = System.Random.random rng2 :: (Int, System.Random.StdGen)
      M.Bounds width height = bounds
      -- width = M.maxWidth bounds
      -- height = M.maxHeight bounds
      randWidth = randX `mod` width
      randHeight = randY `mod` height
      newCoords = [M.Coordinate randWidth randHeight]
   in M.Food newCoords rng3
