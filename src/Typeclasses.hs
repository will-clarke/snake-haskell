module Typeclasses
  ( Drawable(..)
  , Tickable(..)
  ) where

import qualified Model
import qualified Snake
import qualified Brick
import qualified Attr

class Drawable a where
  coords :: a -> [Model.Coordinate]
  widget :: a -> Brick.Widget Model.Name

  -- as we can only `tick` snake, I'm not convinced it's worth having as a typeclass
class Tickable a where
  tick :: Model.Game -> a

instance Typeclasses.Drawable Model.Food where
  widget _ = Brick.withAttr Attr.food $ Brick.str "@"
  coords (Model.Food food _) = food

instance Typeclasses.Drawable Model.Snake where
  widget _ = Brick.withAttr Attr.snakeBody $ Brick.str "*"
  coords (Model.Snake snake) = snake -- or T.getSegments snake

instance Typeclasses.Tickable Model.Snake where
  tick = Snake.moveSnake
