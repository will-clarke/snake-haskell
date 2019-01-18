module Types
  ( State(..)
  , KeyPressed(..)
  , Coord
  , Snake
  , Food
  ) where

import qualified Pointless

-- | Our main game state
data State = State {
    title                :: String,
    keyPressed           :: KeyPressed,
    oscillatingN         :: Int,
    oscillatingDirection :: Pointless.Direction
}

-- | Reflect which keys are being pressed
data KeyPressed = KeyUp | KeyDown | KeyLeft | KeyRight | KeyNone deriving Show

-- | Our lovely snake
type Snake = [Coord]

type Food = [Coord]

type Coord = (Int, Int)
