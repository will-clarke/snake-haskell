module Typeclasses
  ( Drawable(..)
  , Tickable(..)
  ) where

import qualified Types
import qualified Snake

class Drawable a where
  coords :: a -> [Types.Coordinate]
  icon :: a -> Char

class Tickable a where
  tick :: Types.State -> a

instance Typeclasses.Drawable Types.Food where
  icon _ = '@'
  coords (Types.Food food) = food

instance Typeclasses.Drawable Types.Snake where
  icon _ = 'X'
  coords (Types.Snake snake) = snake -- or T.getSegments snake

instance Typeclasses.Tickable Types.Snake where
  tick = Snake.moveSnake

