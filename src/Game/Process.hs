module Game.Process
  ( runStep
  , progress
, handleEvent
  ) where

import qualified Brick        as B
import qualified Graphics.Vty as V
import qualified Pointless
import qualified Types
import qualified Snake

progress :: Types.State -> Types.State
progress g =
  let (n, dir) =
        Pointless.oscillatingNumber
          (Types.oscillatingN g, Types.oscillatingDirection g)
      keyPressed = Types.keyPressed g
      updatedSnake = Snake.tick g
  -- This is some weird way of updating..... {original} {field = updated}
   in Types.exState -- <- this is the {original} example state
        { Types.title = 'x' : Types.title g
        , Types.oscillatingN = n
        , Types.oscillatingDirection = dir
        , Types.snake = updatedSnake
        , Types.keyPressed = keyPressed
        -- , Types.food
        -- , Types.bounds ---- IS IT POSSIBLE TO EXTRACT THIS INFO FROM THE MONAD???
        -- , Types.score
        } -- <- these are the fields I wanna update

runStep :: Types.State -> Types.KeyPressed -> B.EventM() (B.Next Types.State)
runStep g Types.KeyUp    = B.continue $ g { Types.keyPressed = Types.KeyUp }
runStep g Types.KeyDown  = B.continue $ g { Types.keyPressed = Types.KeyDown }
runStep g Types.KeyRight = B.continue $ g { Types.keyPressed = Types.KeyRight }
runStep g Types.KeyLeft  = B.continue $ g { Types.keyPressed = Types.KeyLeft }
runStep g _              = B.continue g

handleEvent :: Types.State -> B.BrickEvent () () -> B.EventM () (B.Next Types.State)
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey V.KUp [])) = runStep (progress g) Types.KeyUp
handleEvent g (B.VtyEvent (V.EvKey V.KDown [])) = runStep (progress g) Types.KeyDown
handleEvent g (B.VtyEvent (V.EvKey V.KLeft [])) = runStep (progress g) Types.KeyLeft
handleEvent g (B.VtyEvent (V.EvKey V.KRight [])) = runStep (progress g) Types.KeyRight
  -- TODO: get these few working!
handleEvent g (B.VtyEvent (V.EvResize w h)) = runStep (progress g) Types.KeyRight
handleEvent g (B.VtyEvent (V.EvLostFocus)) = runStep (progress g) Types.KeyRight
handleEvent g (B.VtyEvent (V.EvGainedFocus)) = runStep (progress g) Types.KeyRight
handleEvent g _ = runStep (progress g) Types.KeyNone
