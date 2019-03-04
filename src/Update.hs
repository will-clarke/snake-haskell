module Update
  (
    handleEvent
  ) where

import qualified Brick        as B
import qualified Food
import qualified Game
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
          _ -> "You're super awesome :D"
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

dead :: M.Game -> Bool
dead game =
  let snake = M.snake game
      (snakeHead:snakeTail) = M.getSegments snake
      x = M.x snakeHead
      y = M.y snakeHead
      bounds= M.bounds game
      maxWidth = M.maxWidth bounds
      maxHeight = M.maxHeight bounds
      headIsOnBody = elem snakeHead snakeTail
   in or [x < 0, y < 0, x > (maxWidth - 1), y > (maxHeight - 1), headIsOnBody]

--- TODO: Refactor this horrific mess :|
handleEvent :: M.State -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt $ M.Playing game
handleEvent (M.Playing game) _ | dead game = B.continue $ M.GameOver $ M.toAttempt game
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ M.Playing $ updateGameDirection game M.North
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey V.KDown [])) =  B.continue $ M.Playing $ updateGameDirection game M.South
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey V.KLeft [])) =  B.continue $ M.Playing $ updateGameDirection game M.West
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey V.KRight [])) =  B.continue $ M.Playing $ updateGameDirection game M.East
handleEvent (M.Playing game) (B.VtyEvent (V.EvResize w h)) = B.continue $ M.Playing $ game { M.bounds = M.Bounds w h }
handleEvent (M.Playing game) (B.VtyEvent V.EvLostFocus) =  B.continue $ M.Paused game
handleEvent (M.Playing game) (B.VtyEvent V.EvGainedFocus) = B.continue $ M.Paused game
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey (V.KChar 'p') [])) = B.continue $ M.Paused game
handleEvent (M.Playing game) (B.VtyEvent (V.EvKey (V.KChar ' ') [])) =  B.continue (M.Playing $ tick game)
handleEvent (M.Playing game) (B.AppEvent M.Tick) =  B.continue (M.Playing $ tick game)
handleEvent (M.Playing game) _ = B.continue $ M.Playing game

handleEvent (M.StartScreen options) (B.AppEvent M.Tick) = B.continue $ M.StartScreen options
handleEvent (M.StartScreen options) (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt $ M.StartScreen options
handleEvent (M.StartScreen (M.Options seed bounds graphics)) _ = B.continue $ M.Playing (Game.initialGame seed bounds graphics)

handleEvent (M.GameOver score') (B.AppEvent M.Tick) = B.continue $ M.GameOver score'
handleEvent (M.GameOver score') _ = B.halt $ M.GameOver score'

handleEvent (M.Paused game) (B.AppEvent M.Tick) = B.continue $ M.Paused game
handleEvent (M.Paused game) _ = B.continue $ M.Playing game


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
