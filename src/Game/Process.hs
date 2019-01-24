module Game.Process
  ( runStep
  , progress
, handleEvent
  ) where

import qualified Brick        as B
import qualified Graphics.Vty as V
import qualified Snake
import qualified Types

progress :: Types.State -> Types.State
progress state =
  let keyPressed = Types.keyPressed state
      updatedSnake = Snake.tick state
      newTitle = 'x' : Types.title state
  -- This is some weird way of updating..... {original} {field = updated}
   in Types.exState -- <- this is the {original} example state
        { Types.title = newTitle
        , Types.snake = updatedSnake
        , Types.keyPressed = keyPressed
        -- , Types.food
        -- , Types.bounds ---- IS IT POSSIBLE TO EXTRACT THIS INFO FROM THE MONAD???
        -- , Types.score
        } -- <- these are the fields I wanna update

runStep :: Types.State -> Types.KeyPressed -> B.EventM Types.Name (B.Next Types.State)
runStep state Types.KeyUp    = B.continue $ state { Types.keyPressed = Types.KeyUp }
runStep state Types.KeyDown  = B.continue $ state { Types.keyPressed = Types.KeyDown }
runStep state Types.KeyRight = B.continue $ state { Types.keyPressed = Types.KeyRight }
runStep state Types.KeyLeft  = B.continue $ state { Types.keyPressed = Types.KeyLeft }
runStep state _              = B.continue state

handleEvent :: Types.State -> B.BrickEvent Types.Name () -> B.EventM Types.Name (B.Next Types.State)
handleEvent state (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt state
  -- TODO: Sort out this hack --- key up != key down! Flip the vertical orintation in the draw function.
handleEvent state (B.VtyEvent (V.EvKey V.KUp [])) = runStep (progress state) Types.KeyDown
handleEvent state (B.VtyEvent (V.EvKey V.KDown [])) = runStep (progress state) Types.KeyUp
handleEvent state (B.VtyEvent (V.EvKey V.KLeft [])) = runStep (progress state) Types.KeyLeft
handleEvent state (B.VtyEvent (V.EvKey V.KRight [])) = runStep (progress state) Types.KeyRight
handleEvent state (B.VtyEvent (V.EvResize w h)) =
  runStep
    (progress (state {Types.bounds = Types.Bounds w h}))
    (Types.keyPressed state)
handleEvent state (B.VtyEvent V.EvLostFocus) = runStep (progress state) Types.KeyRight
handleEvent state (B.VtyEvent V.EvGainedFocus) = runStep (progress state) Types.KeyRight
handleEvent state _ = runStep (progress state) Types.KeyNone
