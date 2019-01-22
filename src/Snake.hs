module Snake
  ( Drawable(..)
  , Tickable(..)
  ) where

import qualified Types as T
import qualified Pointless

class Drawable a where
  coords :: a -> [T.Coordinate]
  icon :: a -> Char

class Tickable a where
  tick :: a -> a

instance Drawable T.Snake where
  icon _ = 'X'
  coords (T.Snake snake) = snake -- or T.getSegments snake

instance Tickable T.Snake where
  tick state = state
