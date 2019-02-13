module Main where

import qualified Attr
import qualified Brick               as B
import qualified Brick.BChan
import           Brick.Main          ()
import qualified Control.Concurrent
import qualified Control.Monad
import qualified Draw
import qualified Model
import qualified Options.Applicative as O
import qualified Game
import qualified Update

app :: B.App Model.Game Model.Tick Model.Name

app = B.App
  { B.appDraw         = Draw.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Update.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const Attr.defaultMap
  }

  -- TODO: OH HEY.
  -- It'd sure be swell if we could
  -- 1. add a game over screen
  -- 2. implement DEATH
main :: IO Model.Game
main = do
  let delay = 100000
  (Options seed graphics') <- O.execParser fullopts
  chan <- Brick.BChan.newBChan 10
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Model.Tick
      Control.Concurrent.threadDelay delay
  B.customMain Draw.defaultVty (Just chan) app $ startingGame seed graphics'

startingGame :: Int -> Model.Graphics -> Model.Game
startingGame seed = Game.initialGame seed bounds
  where
    bounds = Model.Bounds {Model.maxHeight = 20, Model.maxWidth = 60}

fullopts :: O.ParserInfo Options
fullopts = O.info (O.helper <*> parsedOptions)
  (  O.fullDesc
  <> O.header "Snake - majestically displayed in ASCII glory" )

data Options = Options
  { startSeed :: Int
  , graphics  :: Model.Graphics
  } deriving (Show)

parsedOptions :: O.Parser Options
parsedOptions =
  Options <$>
  O.option
    O.auto
    (O.long "seed" <> O.help "Give a starting seed" <> O.showDefault <>
     O.value 42 <>
     O.short 's' <>
     O.metavar "INT") <*>
  -- make this a switch:
  O.option
    O.auto
    (O.long "graphics" <> O.help "Choose graphics: [Simple | Complex]" <>
     O.showDefault <>
     O.value Model.Complex <>
     O.short 'g')
