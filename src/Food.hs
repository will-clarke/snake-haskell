module Food
  ( exFood
  ) where
import qualified Typeclasses
import qualified Types as T

exFood :: T.Food
exFood = T.Food { T.getFood = [T.Coordinate{T.x = 5, T.y = 6}]}
