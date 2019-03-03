module Main where

import qualified Attr
import qualified Brick               as B
import qualified Brick.BChan
import           Brick.Main          ()
import qualified Control.Concurrent
import qualified Control.Monad
import qualified Draw
import qualified Game
import qualified Leaderboard
import qualified Model
import qualified Options.Applicative as O
import qualified Update

app :: B.App Model.State Model.Tick Model.Name

app = B.App
  { B.appDraw         = Draw.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Update.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const Attr.defaultMap
  }

playGame :: Model.Options -> IO Model.State
playGame options = do
  let delay = 100000
  chan <- Brick.BChan.newBChan 10
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Model.Tick
      Control.Concurrent.threadDelay delay
  B.customMain Draw.defaultVty (Just chan) app $
    Model.StartScreen options

main :: IO ()
main = do
  options <- O.execParser fullopts :: IO Model.Options
  game <- playGame options
  handleEndGame game

handleEndGame :: Model.State -> IO ()
handleEndGame state@(Model.GameOver attempt) = do
  Leaderboard.writeLeaderboard
  -- file <- Leaderboard.getLeaderboardFile
  -- beLeaderboard <- Leaderboard.getLeaderboard
  -- writeFile file $ Leaderboard.serialiseAttempt attempt
  B.simpleMain $ Draw.gameOverWidget attempt maybeLeaderboard
handleEndGame _ = return ()
-- handleEndGame _ = return (Model.GameOver (Model.Attempt (Model.League (Model.Bounds 1 1)) (Model.Score 1)))

startingGame :: Int -> Model.Graphics -> Model.Game
startingGame seed = Game.initialGame seed bounds
  where
    bounds = Model.Bounds {Model.maxHeight = 20, Model.maxWidth = 60}

fullopts :: O.ParserInfo Model.Options
fullopts = O.info (O.helper <*> parsedOptions)
  (  O.fullDesc
  <> O.header "Snake - majestically displayed in ASCII glory" )

parsedOptions :: O.Parser Model.Options
parsedOptions =
  Model.Options <$>
  O.option
    O.auto
    (O.long "seed" <> O.help "Give a starting seed" <> O.showDefault <>
     O.value 42 <>
     O.short 's' <>
     O.metavar "INT") <*>
  O.option
    O.auto
    (O.long "bounds" <> O.help "Size of the grid" <> O.showDefault <>
     O.value (Model.Bounds 40 20) <>
     O.short 'b') <*>
  O.option
    O.auto
    (O.long "graphics" <> O.help "Choose graphics: [Simple | Complex]" <>
     O.showDefault <>
     O.value Model.Complex <>
     O.short 'g')
