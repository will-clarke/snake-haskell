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
  let keyPressed = T.keyPressed state
      updatedSnake = Typeclasses.tick state
      (updatedFood, updatedStdGen) = Food.update state
      newTitle = 'x' : T.title state
      newBounds = T.bounds state
      food = T.food state
      snake = T.snake state
      newScore = Food.calculateScore (T.score state) snake food
   in T.State
        { T.title = newTitle
        , T.snake = updatedSnake
        , T.food = updatedFood
        , T.keyPressed = keyPressed
        , T.direction = directionFromKeyPress keyPressed
        , T.previousDirection = T.direction state
        , T.score = newScore
        , T.bounds = newBounds
        , T.randomGenerator = updatedStdGen
        }
          -- <- these are the fields I wanna update

directionFromKeyPress :: T.KeyPressed -> T.Direction
directionFromKeyPress T.KeyUp    = T.North
directionFromKeyPress T.KeyDown  = T.South
directionFromKeyPress T.KeyLeft  = T.West
directionFromKeyPress T.KeyRight = T.East
  -- TODO: Is this a bad idea?:
directionFromKeyPress T.KeyNone  = T.North

handleEvent :: T.State -> B.BrickEvent T.Name T.Tick -> B.EventM T.Name (B.Next T.State)
handleEvent state (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt state
  -- TODO: Sort out this hack --- key up != key down! Flip the vertical orintation in the draw function.
handleEvent state (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ state { T.keyPressed = T.KeyDown }
handleEvent state (B.VtyEvent (V.EvKey V.KDown [])) =  B.continue $ state { T.keyPressed = T.KeyUp }
handleEvent state (B.VtyEvent (V.EvKey V.KLeft [])) =  B.continue $ state { T.keyPressed = T.KeyLeft }
handleEvent state (B.VtyEvent (V.EvKey V.KRight [])) =  B.continue $ state { T.keyPressed = T.KeyRight }
handleEvent state (B.VtyEvent (V.EvResize w h)) = B.continue $ state { T.bounds = T.Bounds w h }
  -- TODO: implement pausing & resuming
handleEvent state (B.VtyEvent V.EvLostFocus) =  B.continue state
handleEvent state (B.VtyEvent V.EvGainedFocus) = B.continue state
handleEvent state (B.VtyEvent (V.EvKey (V.KChar ' ') [])) =  B.continue (tick state)
handleEvent state (B.AppEvent T.Tick) =  B.continue (tick state)
handleEvent state _ = B.continue state
