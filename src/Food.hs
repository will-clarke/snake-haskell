module Food
  ( exFood
  , snakeEating
  , calculateScore
  , reseed
  ) where

import qualified Types       as T

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
reseed :: T.State -> T.Food
reseed state =
  let currentFood = T.food state
      snake = T.snake state
      eating = snakeEating snake currentFood
   in if eating
        then generateRandomFood state
        else currentFood

generateRandomFood :: T.State -> T.Food
generateRandomFood _ = exFood
