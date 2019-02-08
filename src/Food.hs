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
isFoodAt coord food = elem coord $ M.getFood food

snakeEating :: M.Snake -> M.Food -> Bool
snakeEating s = isFoodAt (head $ M.getSegments s)

-- What happens when we tick food...
update :: M.State -> (M.Food, System.Random.StdGen)
update state =
  let currentFood = M.food state
      snake = M.snake state
      stdGen = M.randomGenerator state
      bounds = M.bounds state
      eating = snakeEating snake currentFood
      extractFoodAndRNG :: M.State -> (M.Food, System.Random.StdGen)
      extractFoodAndRNG s = (M.food s, M.randomGenerator s)
   in if eating
        then generateRandomFood stdGen bounds
        else extractFoodAndRNG state

pure :: [M.Coordinate] -> M.Food
pure = M.Food

generateRandomFood :: System.Random.StdGen -> M.Bounds -> (M.Food, System.Random.StdGen)
generateRandomFood rng bounds =
  let (randX, rng2) = System.Random.random rng :: (Int, System.Random.StdGen)
      (randY, rng3) = System.Random.random rng2 :: (Int, System.Random.StdGen)
      width = M.maxWidth $ bounds
      height = M.maxHeight $ bounds
      randWidth = randX `mod` width
      randHeight = randY `mod` height
      newFood = Food.pure [M.Coordinate randWidth randHeight]
   in (newFood, rng3)
