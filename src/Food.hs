module Food
  ( exFood
  , isFoodAt
  ) where

import qualified Types       as T

exFood :: T.Food
exFood = T.Food { T.getFood = [T.Coordinate{T.x = 5, T.y = 6}]}

-- calculateScore ::
-- snakeHeadOnFood (head snakeCoords) food

isFoodAt :: T.Coordinate -> T.Food -> Bool
isFoodAt coord food = elem coord $ T.getFood food
