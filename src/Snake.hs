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

-- nextHeadCoords :: T.Coordinate -> T.Direction -> T.Direction -> T.Coordinate
-- nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.North T.South = T.Coordinate{T.x = 10, T.y = 10}
-- nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.North _ = T.Coordinate{T.x = x, T.y = y + 1}
-- -- nextHeadCoords coords T.South T.North = coords
-- nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.South _ = T.Coordinate{T.x = x, T.y = y - 1}
-- -- nextHeadCoords coords T.West T.East = coords
-- nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.West _ = T.Coordinate{T.x = x - 1, T.y = y}
-- -- nextHeadCoords coords T.East T.West = coords
-- nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.East _ = T.Coordinate{T.x = x + 1, T.y = y}

nextHeadCoords :: T.Coordinate -> T.Direction -> T.Direction -> T.Coordinate
nextHeadCoords c  T.North T.South = moveSouth c
nextHeadCoords c  T.North _ = moveNorth c
nextHeadCoords c  T.East T.West = moveWest c
nextHeadCoords c  T.East _ = moveEast c
nextHeadCoords c  T.South T.North = moveNorth c
nextHeadCoords c  T.South _ = moveSouth c
nextHeadCoords c  T.West T.East = moveEast c
nextHeadCoords c  T.West _ = moveWest c

moveSouth :: T.Coordinate -> T.Coordinate
moveSouth (T.Coordinate x y) = T.Coordinate x (y - 1)

moveNorth :: T.Coordinate -> T.Coordinate
moveNorth (T.Coordinate x y) = T.Coordinate x (y + 1)

moveEast :: T.Coordinate -> T.Coordinate
moveEast (T.Coordinate x y) = T.Coordinate (x + 1) y

moveWest :: T.Coordinate -> T.Coordinate
moveWest (T.Coordinate x y) = T.Coordinate (x - 1) y
