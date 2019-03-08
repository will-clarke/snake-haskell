module Main where

import qualified Attr
import qualified Brick                  as B
import qualified Brick.BChan
import           Brick.Main             ()
import qualified Control.Concurrent
import qualified Control.Concurrent.STM
import qualified Control.Monad
import qualified Draw
import qualified Leaderboard
import qualified Model
import qualified Options.Applicative    as O
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
  tvar <- Control.Concurrent.STM.newTVarIO 0
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Model.Tick
      n <- Control.Concurrent.STM.readTVarIO tvar
      Control.Concurrent.threadDelay (delay - (n * 10000))
  B.customMain Draw.defaultVty (Just chan) app $
    Model.StartScreen options tvar

main :: IO ()
main = do
  options <- O.execParser fullopts :: IO Model.Options
  game <- playGame options
  handleEndGame game

handleEndGame :: Model.State -> IO ()
handleEndGame (Model.GameOver attempt) = do
  leaderboard <- Leaderboard.writeLeaderboard attempt
  B.simpleMain $ Draw.gameOverWidget attempt leaderboard
handleEndGame _ = return ()

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
    (O.long "graphics" <> O.help "Choose graphics: [Simple | Complex]" <>
     O.value Model.Complex <>
     O.short 'g') <*>
  O.option
    O.auto
    (O.long "width" <> O.help "Grid width" <> O.showDefault <> O.value 40 <>
     O.short 'w') <*>
  O.option
    O.auto
    (O.long "height" <> O.help "Grid height" <> O.showDefault <> O.value 20 <>
     O.short 'h')
