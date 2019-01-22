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
progress g =
  let (n, dir) =
        Pointless.oscillatingNumber
          (Types.oscillatingN g, Types.oscillatingDirection g)
  -- This is some weird way of updating..... {original} {field = updated}
   in Types.exState -- <- this is the {original} example state
        { Types.title = 'x' : Types.title g
        , Types.oscillatingN = n
        , Types.oscillatingDirection = dir
        } -- <- these are the fields I wanna update

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
