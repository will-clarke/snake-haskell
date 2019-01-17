module Game
  ( State(..)
  , handleEvent
  , newState
  ) where

import qualified Brick        as B
import qualified Graphics.Vty as V
import qualified Pointless
import qualified SnakeIO

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

progress :: State -> State
  -- TODO: There must be a better way to update games.. Maybe look into lenses?
  -- o_O
progress g =
  let (n, dir) =
        Pointless.oscillatingNumber (oscillatingN g, oscillatingDirection g)
   in State ('x' : title g) (keyPressed g) n dir

-- TODO: Can we move this to the UI lib?
handleEvent :: Game.State -> B.BrickEvent () () -> B.EventM () (B.Next State)
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey V.KUp [])) = runStep g SnakeIO.KeyUp
handleEvent g (B.VtyEvent (V.EvKey V.KDown [])) = runStep g SnakeIO.KeyDown
handleEvent g (B.VtyEvent (V.EvKey V.KLeft [])) = runStep g SnakeIO.KeyLeft
handleEvent g (B.VtyEvent (V.EvKey V.KRight [])) = runStep g SnakeIO.KeyRight
handleEvent g _ = runStep (progress g) SnakeIO.KeyNone

runStep :: State -> SnakeIO.KeyPressed -> B.EventM() (B.Next State)
runStep g SnakeIO.KeyUp    = B.continue $ g { keyPressed = SnakeIO.KeyUp }
runStep g SnakeIO.KeyDown  = B.continue $ g { keyPressed = SnakeIO.KeyDown }
runStep g SnakeIO.KeyRight = B.continue $ g { keyPressed = SnakeIO.KeyRight }
runStep g SnakeIO.KeyLeft  = B.continue $ g { keyPressed = SnakeIO.KeyLeft }
runStep g _                = B.continue g
