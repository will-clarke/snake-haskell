module Snake
  ( exSnake
  , moveSnake
  ) where

import qualified Types       as T

-- example snake for mucking around with
exSnake :: T.Snake
exSnake = T.Snake { T.getSegments = [T.Coordinate{T.x = 30, T.y = 15}]}

moveSnake :: T.State -> T.Snake
moveSnake state =
  let snake = T.snake state
      direction = T.keyPressed state
   in slither snake direction

slither :: T.Snake -> T.KeyPressed -> T.Snake
slither (T.Snake snake) key =
  T.Snake (nextHeadCoords (head snake) key : restOfSnake snake)
  where
    restOfSnake s =
      if length s >= 10
        then init s
        else s

nextHeadCoords :: T.Coordinate -> T.KeyPressed -> T.Coordinate
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyUp = T.Coordinate{T.x = x, T.y = y + 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyDown = T.Coordinate{T.x = x, T.y = y - 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyLeft = T.Coordinate{T.x = x - 1, T.y = y}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyRight = T.Coordinate{T.x = x + 1, T.y = y}
nextHeadCoords coordinates T.KeyNone = coordinates
