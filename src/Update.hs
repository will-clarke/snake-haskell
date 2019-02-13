module Update
  (
    handleEvent
  ) where

import qualified Brick        as B
import qualified Food
import qualified Graphics.Vty as V
import qualified Model        as M
import           Snake        ()
import qualified Typeclasses

tick :: M.Game -> M.Game
tick game =
  let
      updatedSnake = Typeclasses.tick game
      updatedFood = Food.update game
      food = M.food game
      snake = M.snake game
      newScore = Food.calculateScore (M.score game) snake food
      newTitle =
        case newScore of
          0 -> "Welcome to Snake!!!!1!"
          1 -> "Nice"
          2 -> "Keep On going"
          3 -> "You're winning"
          _ -> "You're god-like"
   in M.Game
        { M.title = newTitle
        , M.snake = updatedSnake
        , M.food = updatedFood
        , M.direction = M.direction game
        , M.previousDirection = M.direction game
        , M.score = newScore
        , M.bounds = M.bounds game
        , M.graphics = M.graphics game
        }

handleEvent :: M.Game -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.Game)
handleEvent game (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt game
handleEvent game (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ updateGameDirection game M.North
handleEvent game (B.VtyEvent (V.EvKey V.KDown [])) =  B.continue $ updateGameDirection game M.South
handleEvent game (B.VtyEvent (V.EvKey V.KLeft [])) =  B.continue $ updateGameDirection game M.West
handleEvent game (B.VtyEvent (V.EvKey V.KRight [])) =  B.continue $ updateGameDirection game M.East
handleEvent game (B.VtyEvent (V.EvResize w h)) = B.continue $ game { M.bounds = M.Bounds w h }
handleEvent game (B.VtyEvent V.EvLostFocus) =  B.continue game
handleEvent game (B.VtyEvent V.EvGainedFocus) = B.continue game
handleEvent game (B.VtyEvent (V.EvKey (V.KChar ' ') [])) =  B.continue (tick game)
handleEvent game (B.AppEvent M.Tick) =  B.continue (tick game)
handleEvent game _ = B.continue game

updateGameDirection :: M.Game -> M.Direction -> M.Game
updateGameDirection game direction = game {M.direction = newDirection direction oldDirection}
  where oldDirection = M.previousDirection game

newDirection :: M.Direction -> M.Direction -> M.Direction
newDirection M.North M.South = M.South
newDirection M.North _       = M.North
newDirection M.South M.North=M.North
newDirection M.South _       = M.South
newDirection M.East M.West   = M.West
newDirection M.East _        = M.East
newDirection M.West M.East   = M.East
newDirection M.West _        = M.West

