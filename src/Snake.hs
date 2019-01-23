module Snake
  ( Drawable(..)
  , Tickable(..)
  ) where

import qualified Types as T

class Drawable a where
  coords :: a -> [T.Coordinate]
  icon :: a -> Char

class Tickable a where
  tick :: T.State -> a

instance Drawable T.Snake where
  icon _ = 'X'
  coords (T.Snake snake) = snake -- or T.getSegments snake

instance Drawable T.Food where
  icon _ = '@'
  coords (T.Food food) = food

instance Tickable T.Snake where
  tick = moveSnake

moveSnake :: T.State -> T.Snake
moveSnake state =
  let snake = T.snake state
      direction = T.keyPressed state
   in slither snake direction

slither :: T.Snake -> T.KeyPressed -> T.Snake
slither (T.Snake s) key = T.Snake (nextHeadCoords (head s) key : s)

nextHeadCoords :: T.Coordinate -> T.KeyPressed -> T.Coordinate
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyUp = T.Coordinate{T.x = x, T.y = y + 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyDown = T.Coordinate{T.x = x, T.y = y - 1}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyLeft = T.Coordinate{T.x = x - 1, T.y = y}
nextHeadCoords T.Coordinate{T.x = x, T.y = y} T.KeyRight = T.Coordinate{T.x = x + 1, T.y = y}
nextHeadCoords coordinates T.KeyNone = coordinates

-- snakeHead :: T.Snake -> T.Coordinate
-- snakeHead (T.Snake s) = head s

-- snakeTail :: T.Snake -> [T.Coordinate]
-- snakeTail (T.Snake s) = tail s

