module SnakeIO
  ( KeyPressed(..),
  ) where

-- | Reflect which keys are being pressed
data KeyPressed = KeyUp | KeyDown | KeyLeft | KeyRight | KeyNone deriving Show

