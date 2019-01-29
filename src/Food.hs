module Food
  ( exFood
  , snakeEating
  , calculateScore
  , update
  ) where

import qualified Types       as T
import qualified System.Random

exFood :: T.Food
exFood = T.Food { T.getFood = [T.Coordinate{T.x = 5, T.y = 6}]}

calculateScore :: Int -> T.Snake -> T.Food -> Int
calculateScore oldScore snake food =
  if eating
    then oldScore + 1
    else oldScore
  where
    eating = snakeEating snake food

isFoodAt :: T.Coordinate -> T.Food -> Bool
isFoodAt coord food = elem coord $ T.getFood food

snakeEating :: T.Snake -> T.Food -> Bool
snakeEating s = isFoodAt (head $ T.getSegments s)

-- What happens when we tick food...
update :: T.State -> (T.Food, System.Random.StdGen)
update state =
  let currentFood = T.food state
      snake = T.snake state
      eating = snakeEating snake currentFood
      extractFoodAndRNG :: T.State -> (T.Food, System.Random.StdGen)
      extractFoodAndRNG s = (T.food s, T.randomGenerator s)
   in if eating
        then generateRandomFood state
        else extractFoodAndRNG state

pure :: [T.Coordinate] -> T.Food
pure = T.Food

generateRandomFood :: T.State -> (T.Food, System.Random.StdGen)
generateRandomFood state =
  let rng = T.randomGenerator state :: System.Random.StdGen
      (randX, rng2) = System.Random.random rng :: (Int, System.Random.StdGen)
      (randY, rng3) = System.Random.random rng2 :: (Int, System.Random.StdGen)
      width = T.maxWidth $ T.bounds state
      height = T.maxHeight $ T.bounds state
      randWidth = randX `mod` width
      randHeight = randY `mod` height
      newFood = Food.pure [T.Coordinate randWidth randHeight]
   in (newFood, rng3)
