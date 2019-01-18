module Game.State
  ( State(..)
  , newState
  ) where

import qualified SnakeIO
import qualified Pointless


-- | Our main game state
data State = State {
    title                :: String,
    keyPressed           :: SnakeIO.KeyPressed,
    oscillatingN         :: Int,
    oscillatingDirection :: Pointless.Direction
}

newState :: State
newState =
  State
    { title = "Super Funky Title"
    , keyPressed = SnakeIO.KeyNone
    , oscillatingN = 1
    , oscillatingDirection = Pointless.L
    }
