module Snake
  ( exSnake
  , moveSnake
  ) where

import qualified Food
import qualified Types     as T

exSnake :: T.Snake
exSnake = T.Snake { T.getSegments = [T.Coordinate{T.x = 30, T.y = 15},T.Coordinate{T.x = 31, T.y = 15},T.Coordinate{T.x = 32, T.y = 15} ]}

moveSnake :: T.State -> T.Snake
moveSnake state =
  let snake = T.snake state
      food = T.food state
      snakeCoords = T.getSegments snake
      direction = T.direction state
      previousDirection = T.previousDirection state
      isEating = Food.snakeEating snake food
      restOfSnake =
        if isEating
          then snakeCoords
          else init snakeCoords
   in T.Snake (nextHeadCoords (head snakeCoords) direction previousDirection : restOfSnake)

nextHeadCoords :: T.Coordinate -> T.Direction -> T.Direction -> T.Coordinate
-- nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.North T.South = T.Coordinate{T.x = x, T.y = y - 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.North _ = T.Coordinate{T.x = x, T.y = y + 1}
-- nextHeadCoords coords T.South T.North = coords
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.South _ = T.Coordinate{T.x = x, T.y = y - 1}
-- nextHeadCoords coords T.West T.East = coords
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.West _ = T.Coordinate{T.x = x - 1, T.y = y}
-- nextHeadCoords coords T.East T.West = coords
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.East _ = T.Coordinate{T.x = x + 1, T.y = y}
