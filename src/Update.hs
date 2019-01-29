module Update
  (
    handleEvent
  ) where

import qualified Brick        as B
import qualified Food
import qualified Graphics.Vty as V
import           Snake        ()
import qualified Typeclasses
import qualified Types        as T

tick :: T.State -> T.State
tick state =
  let
      updatedSnake = Typeclasses.tick state
      (updatedFood, updatedStdGen) = Food.update state
      newBounds = T.bounds state
      food = T.food state
      snake = T.snake state
      newScore = Food.calculateScore (T.score state) snake food
      newTitle =
        case newScore of
          0 -> "Welcome to Snake!!!!1!"
          1 -> "Nice"
          2 -> "Keep On going"
          3 -> "You're winning"
          _ -> "You're god-like"
   in T.State
        { T.title = newTitle
        , T.snake = updatedSnake
        , T.food = updatedFood
        , T.direction = T.direction state
        , T.previousDirection = T.direction state
        , T.score = newScore
        , T.bounds = newBounds
        , T.randomGenerator = updatedStdGen
        }

handleEvent :: T.State -> B.BrickEvent T.Name T.Tick -> B.EventM T.Name (B.Next T.State)
handleEvent state (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt state
  -- TODO: Sort out this hack --- key up != key down! Flip the vertical orintation in the draw function.
handleEvent state (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ updateStateDirection state T.North
handleEvent state (B.VtyEvent (V.EvKey V.KDown [])) =  B.continue $ updateStateDirection state T.South
handleEvent state (B.VtyEvent (V.EvKey V.KLeft [])) =  B.continue $ updateStateDirection state T.West
handleEvent state (B.VtyEvent (V.EvKey V.KRight [])) =  B.continue $ updateStateDirection state T.East
handleEvent state (B.VtyEvent (V.EvResize w h)) = B.continue $ state { T.bounds = T.Bounds w h }
  -- TODO: implement pausing & resuming
handleEvent state (B.VtyEvent V.EvLostFocus) =  B.continue state
handleEvent state (B.VtyEvent V.EvGainedFocus) = B.continue state
handleEvent state (B.VtyEvent (V.EvKey (V.KChar ' ') [])) =  B.continue (tick state)
handleEvent state (B.AppEvent T.Tick) =  B.continue (tick state)
handleEvent state _ = B.continue state

updateStateDirection :: T.State -> T.Direction -> T.State
updateStateDirection state direction = state {T.direction = newDirection direction oldDirection}
  where oldDirection = T.previousDirection state


newDirection :: T.Direction -> T.Direction -> T.Direction
newDirection T.North T.South = T.South
newDirection T.North _ = T.North
newDirection T.South T.North= T.North
newDirection T.South _ = T.South
newDirection T.East T.West = T.West
newDirection T.East _ = T.East
newDirection T.West T.East = T.East
newDirection T.West _ = T.West

