module Food
  ( initialFood
  , snakeEating
  , calculateScore
  , update
  ) where

import qualified Model       as M
import qualified System.Random

initialFood :: M.Food
initialFood = M.Food { M.getFood = [M.Coordinate{M.x = 5, M.y = 6}]}

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
      eating = snakeEating snake currentFood
      extractFoodAndRNG :: M.State -> (M.Food, System.Random.StdGen)
      extractFoodAndRNG s = (M.food s, M.randomGenerator s)
   in if eating
        then generateRandomFood state
        else extractFoodAndRNG state

pure :: [M.Coordinate] -> M.Food
pure = M.Food

generateRandomFood :: M.State -> (M.Food, System.Random.StdGen)
generateRandomFood state =
  let rng = M.randomGenerator state :: System.Random.StdGen
      (randX, rng2) = System.Random.random rng :: (Int, System.Random.StdGen)
      (randY, rng3) = System.Random.random rng2 :: (Int, System.Random.StdGen)
      width = M.maxWidth $ M.bounds state
      height = M.maxHeight $ M.bounds state
      randWidth = randX `mod` width
      randHeight = randY `mod` height
      newFood = Food.pure [M.Coordinate randWidth randHeight]
   in (newFood, rng3)
