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
import qualified Control.Monad.IO.Class
import qualified Control.Concurrent.STM

tick :: M.Game -> M.Game
tick game =
  let
      updatedSnake = Typeclasses.tick game
      updatedFood = Food.update game
      food = M.getFood game
      snake = M.getSnake game
      newScore = Food.calculateScore (M.getScore game) snake food
      newGetTitle =
        case newScore of
          0 -> "Welcome to Snake!!!!1!"
          1 -> "Nice"
          2 -> "Keep On going"
          3 -> "You're winning"
          4 -> "Omg. This is probably a high score or something"
          5 -> "*sheds tears of joy*"
          6 -> "Are you cheating?!"
          _ -> "You're super awesome :D"
   in M.Game
        { M.getTitle = newGetTitle
        , M.getSnake = updatedSnake
        , M.getFood = updatedFood
        , M.direction = M.direction game
        , M.previousDirection = M.direction game
        , M.getScore = newScore
        , M.getBounds = M.getBounds game
        , M.getGraphics = M.getGraphics game
        , M.getSpeedControl = M.getSpeedControl game
        }

dead :: M.Game -> Bool
dead game =
  let getSnake = M.getSnake game
      (getSnakeHead:getSnakeTail) = M.getSegments getSnake
      x = M.x getSnakeHead
      y = M.y getSnakeHead
      getBounds= M.getBounds game
      getMaxWidth = M.getMaxWidth getBounds
      getMaxHeight = M.getMaxHeight getBounds
      headIsOnBody = elem getSnakeHead getSnakeTail
   in or [x < 0, y < 0, x > (getMaxWidth - 1), y > (getMaxHeight - 1), headIsOnBody]

tickGame :: M.Game -> B.EventM n (B.Next M.State)
tickGame game = do
  Control.Monad.IO.Class.liftIO $
    Control.Concurrent.STM.atomically $
    Control.Concurrent.STM.writeTVar (M.getSpeedControl game) (M.getScore game)
  B.continue (M.Playing $ tick game)

handleEvent :: M.State -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)
handleEvent (M.Playing game) event = handlePlaying game event
handleEvent (M.StartScreen options tvar) event = handleStartScreen options tvar event
handleEvent (M.Paused game) event = handlePaused game event
handleEvent (M.GameOver attempt) event = handleGameOver attempt event

handlePlaying :: M.Game -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt $ M.Playing game
handlePlaying game _ | dead game = B.continue $ M.GameOver $ M.toAttempt game
handlePlaying game (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ M.Playing $ updateGameDirection game M.North
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar 'w') [])) = B.continue $ M.Playing $ updateGameDirection game M.North
handlePlaying game (B.VtyEvent (V.EvKey V.KDown [])) =  B.continue $ M.Playing $ updateGameDirection game M.South
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar 's') [])) =  B.continue $ M.Playing $ updateGameDirection game M.South
handlePlaying game (B.VtyEvent (V.EvKey V.KLeft [])) =  B.continue $ M.Playing $ updateGameDirection game M.West
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar 'a') [])) =  B.continue $ M.Playing $ updateGameDirection game M.West
handlePlaying game (B.VtyEvent (V.EvKey V.KRight [])) =  B.continue $ M.Playing $ updateGameDirection game M.East
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar 'd') [])) =  B.continue $ M.Playing $ updateGameDirection game M.East
handlePlaying game (B.VtyEvent V.EvLostFocus) =  B.continue $ M.Paused game
handlePlaying game (B.VtyEvent V.EvGainedFocus) = B.continue $ M.Paused game
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar 'p') [])) = B.continue $ M.Paused game
handlePlaying game (B.VtyEvent (V.EvKey (V.KChar ' ') [])) =  B.continue (M.Playing $ tick game)
handlePlaying game (B.AppEvent M.Tick) = tickGame game
handlePlaying game _ = B.continue $ M.Playing game

handleStartScreen :: M.Options -> Control.Concurrent.STM.TVar Int -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)

handleStartScreen options tvar (B.AppEvent M.Tick) = B.continue $ M.StartScreen options tvar
handleStartScreen options tvar (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt $ M.StartScreen options tvar
handleStartScreen options tvar _ = B.continue $ M.Playing (Game.initialGame options tvar)


handleGameOver :: M.Attempt -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)
handleGameOver attempt (B.AppEvent M.Tick) = B.continue $ M.GameOver attempt
handleGameOver attempt _ = B.halt $ M.GameOver attempt

handlePaused :: M.Game -> B.BrickEvent M.Name M.Tick -> B.EventM M.Name (B.Next M.State)
handlePaused game (B.AppEvent M.Tick) = B.continue $ M.Paused game
handlePaused game _ = B.continue $ M.Playing game


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
