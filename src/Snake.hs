module Snake
  ( exSnake
  , moveSnake
  ) where

import qualified Food
import qualified Types     as T

exSnake :: T.Snake
exSnake = T.Snake { T.getSegments = [T.Coordinate{T.x = 30, T.y = 15}]}

moveSnake :: T.State -> T.Snake
moveSnake state =
  let snake = T.snake state
      food = T.food state
      snakeCoords = T.getSegments snake
      direction = T.direction state
      isEating = Food.snakeEating snake food
      restOfSnake =
        if isEating
          then snakeCoords
          else init snakeCoords
   in T.Snake (nextHeadCoords (head snakeCoords) direction : restOfSnake)

nextHeadCoords :: T.Coordinate -> T.Direction -> T.Coordinate
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.North = T.Coordinate{T.x = x, T.y = y + 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.South = T.Coordinate{T.x = x, T.y = y - 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.West = T.Coordinate{T.x = x - 1, T.y = y}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.East = T.Coordinate{T.x = x + 1, T.y = y}
