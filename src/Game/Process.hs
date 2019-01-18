module Game.Process
  ( handleEvent
  ) where

import qualified Brick        as B
import qualified Graphics.Vty as V
import qualified Game.State as S
import qualified Pointless
import qualified SnakeIO

progress :: S.State -> S.State
  -- TODO: There must be a better way to update games.. Maybe look into lenses?
  -- o_O
progress g =
  let (n, dir) =
        Pointless.oscillatingNumber (S.oscillatingN g, S.oscillatingDirection g)
   in S.State ('x' : S.title g) (S.keyPressed g) n dir

-- TODO: Can we move this to the UI lib?
handleEvent :: S.State -> B.BrickEvent () () -> B.EventM () (B.Next S.State)
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey V.KUp [])) = runStep g SnakeIO.KeyUp
handleEvent g (B.VtyEvent (V.EvKey V.KDown [])) = runStep g SnakeIO.KeyDown
handleEvent g (B.VtyEvent (V.EvKey V.KLeft [])) = runStep g SnakeIO.KeyLeft
handleEvent g (B.VtyEvent (V.EvKey V.KRight [])) = runStep g SnakeIO.KeyRight
handleEvent g _ = runStep (progress g) SnakeIO.KeyNone

runStep :: S.State -> SnakeIO.KeyPressed -> B.EventM() (B.Next S.State)
runStep g SnakeIO.KeyUp    = B.continue $ g { S.keyPressed = SnakeIO.KeyUp }
runStep g SnakeIO.KeyDown  = B.continue $ g { S.keyPressed = SnakeIO.KeyDown }
runStep g SnakeIO.KeyRight = B.continue $ g { S.keyPressed = SnakeIO.KeyRight }
runStep g SnakeIO.KeyLeft  = B.continue $ g { S.keyPressed = SnakeIO.KeyLeft }
runStep g _                = B.continue g
