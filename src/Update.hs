module Update
  ( runStep
  , progress
, handleEvent
  ) where

import qualified Brick        as B
import qualified Graphics.Vty as V
import Snake()
import qualified Types
import qualified Typeclasses
import qualified State

progress :: Types.State -> Types.State
progress state =
  let keyPressed = Types.keyPressed state
      updatedSnake = Typeclasses.tick state
      newTitle = 'x' : Types.title state
      newBounds = Types.bounds state
  -- This is some weird way of updating..... {original} {field = updated}
   in State.exState -- <- this is the {original} example state
        { Types.title = newTitle
        , Types.snake = updatedSnake
        , Types.keyPressed = keyPressed
        -- , Types.food
        , Types.bounds = newBounds
        -- , Types.score
        } -- <- these are the fields I wanna update


runStep :: Types.State -> Types.KeyPressed -> B.EventM Types.Name (B.Next Types.State)
runStep state Types.KeyUp    = B.continue $ state { Types.keyPressed = Types.KeyUp }
runStep state Types.KeyDown  = B.continue $ state { Types.keyPressed = Types.KeyDown }
runStep state Types.KeyRight = B.continue $ state { Types.keyPressed = Types.KeyRight }
runStep state Types.KeyLeft  = B.continue $ state { Types.keyPressed = Types.KeyLeft }
runStep state _              = B.continue state

handleEvent :: Types.State -> B.BrickEvent Types.Name Types.Tick -> B.EventM Types.Name (B.Next Types.State)
handleEvent state (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt state
  -- TODO: Sort out this hack --- key up != key down! Flip the vertical orintation in the draw function.
handleEvent state (B.VtyEvent (V.EvKey V.KUp [])) = runStep state Types.KeyDown
handleEvent state (B.VtyEvent (V.EvKey V.KDown [])) = runStep state Types.KeyUp
handleEvent state (B.VtyEvent (V.EvKey V.KLeft [])) = runStep state Types.KeyLeft
handleEvent state (B.VtyEvent (V.EvKey V.KRight [])) = runStep state Types.KeyRight
handleEvent state (B.VtyEvent (V.EvResize w h)) =
  runStep (state {Types.bounds = Types.Bounds w h}) Types.KeyNone
  -- TODO: implement pausing & resuming
handleEvent state (B.VtyEvent V.EvLostFocus) = runStep state Types.KeyRight
handleEvent state (B.VtyEvent V.EvGainedFocus) = runStep state Types.KeyRight
handleEvent state (B.VtyEvent (V.EvKey (V.KChar ' ') [])) = runStep (progress state) Types.KeyNone
handleEvent state (B.AppEvent Types.Tick) = runStep (progress state) Types.KeyNone
handleEvent state _ = runStep state Types.KeyNone
