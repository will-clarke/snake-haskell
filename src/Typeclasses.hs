module Typeclasses
  ( Drawable(..)
  , Tickable(..)
  ) where

import qualified Model
import qualified Snake

class Drawable a where
  coords :: a -> [Model.Coordinate]
  icon :: a -> Char

  -- as we can only `tick` snake, I'm not convinced it's worth having as a typeclass
class Tickable a where
  tick :: Model.State -> a

instance Typeclasses.Drawable Model.Food where
  icon _ = '@'
  coords (Model.Food food _) = food

instance Typeclasses.Drawable Model.Snake where
  icon _ = 'X'
  coords (Model.Snake snake) = snake -- or T.getSegments snake

instance Typeclasses.Tickable Model.Snake where
  tick = Snake.moveSnake
