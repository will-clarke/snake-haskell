module Game.Process
  ( runStep
  , progress
, handleEvent
  ) where

import qualified Brick        as B
import qualified Graphics.Vty as V
import qualified Pointless
import qualified Types

progress :: Types.State -> Types.State
  -- TODO: There must be a better way to update games.. Maybe look into lenses?
  -- o_O
progress g =
  let (n, dir) =
        Pointless.oscillatingNumber (Types.oscillatingN g, Types.oscillatingDirection g)
   in Types.State ('x' : Types.title g) (Types.keyPressed g) n dir

runStep :: Types.State -> Types.KeyPressed -> B.EventM() (B.Next Types.State)
runStep g Types.KeyUp    = B.continue $ g { Types.keyPressed = Types.KeyUp }
runStep g Types.KeyDown  = B.continue $ g { Types.keyPressed = Types.KeyDown }
runStep g Types.KeyRight = B.continue $ g { Types.keyPressed = Types.KeyRight }
runStep g Types.KeyLeft  = B.continue $ g { Types.keyPressed = Types.KeyLeft }
runStep g _              = B.continue g


handleEvent :: Types.State -> B.BrickEvent () () -> B.EventM () (B.Next Types.State)
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey V.KUp [])) = runStep g Types.KeyUp
handleEvent g (B.VtyEvent (V.EvKey V.KDown [])) = runStep g Types.KeyDown
handleEvent g (B.VtyEvent (V.EvKey V.KLeft [])) = runStep g Types.KeyLeft
handleEvent g (B.VtyEvent (V.EvKey V.KRight [])) = runStep g Types.KeyRight
handleEvent g _ = runStep (progress g) Types.KeyNone
