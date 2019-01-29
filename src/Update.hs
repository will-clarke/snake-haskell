module Update
  (
    handleEvent
  ) where

import qualified Brick        as B
import qualified Food
import qualified Graphics.Vty as V
import           Snake        ()
import qualified Typeclasses
import qualified Model        as M

tick :: M.State -> M.State
tick state =
  let
      updatedSnake = Typeclasses.tick state
      (updatedFood, updatedStdGen) = Food.update state
      newBounds = M.bounds state
      food = M.food state
      snake = M.snake state
      newScore = Food.calculateScore (M.score state) snake food
      newTitle =
        case newScore of
          0 -> "Welcome to Snake!!!!1!"
          1 -> "Nice"
          2 -> "Keep On going"
          3 -> "You're winning"
          _ -> "You're god-like"
   in M.State
        { M.title = newTitle
        , M.snake = updatedSnake
        , M.food = updatedFood
        , M.direction = M.direction state
        , M.previousDirection = M.direction state
        , M.score = newScore
        , M.bounds = newBounds
        , M.randomGenerator = updatedStdGen
        }

handleEvent :: M.State -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)
handleEvent state (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt state
handleEvent state (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ updateStateDirection state M.North
handleEvent state (B.VtyEvent (V.EvKey V.KDown [])) =  B.continue $ updateStateDirection state M.South
handleEvent state (B.VtyEvent (V.EvKey V.KLeft [])) =  B.continue $ updateStateDirection state M.West
handleEvent state (B.VtyEvent (V.EvKey V.KRight [])) =  B.continue $ updateStateDirection state M.East
handleEvent state (B.VtyEvent (V.EvResize w h)) = B.continue $ state { M.bounds = M.Bounds w h }
handleEvent state (B.VtyEvent V.EvLostFocus) =  B.continue state
handleEvent state (B.VtyEvent V.EvGainedFocus) = B.continue state
handleEvent state (B.VtyEvent (V.EvKey (V.KChar ' ') [])) =  B.continue (tick state)
handleEvent state (B.AppEvent M.Tick) =  B.continue (tick state)
handleEvent state _ = B.continue state

updateStateDirection :: M.State -> M.Direction -> M.State
updateStateDirection state direction = state {M.direction = newDirection direction oldDirection}
  where oldDirection = M.previousDirection state

newDirection :: M.Direction -> M.Direction -> M.Direction
newDirection M.North M.South = M.South
newDirection M.North _ = M.North
newDirection M.South M.North= M.North
newDirection M.South _ = M.South
newDirection M.East M.West = M.West
newDirection M.East _ = M.East
newDirection M.West M.East = M.East
newDirection M.West _ = M.West

