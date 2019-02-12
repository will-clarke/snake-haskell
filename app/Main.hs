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
import qualified State
import qualified Update

app :: B.App Model.State Model.Tick Model.Name

app = B.App
  { B.appDraw         = Draw.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Update.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const Attr.defaultMap
  }

main :: IO Model.State
main = do
  let delay = 100000
  (Options seed graphics) <- O.execParser fullopts
  chan <- Brick.BChan.newBChan 10
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Model.Tick
      Control.Concurrent.threadDelay delay
  B.customMain Draw.defaultVty (Just chan) app $ startingState seed graphics

startingState :: Int -> Model.Graphics -> Model.State
startingState seed graphics = State.initialState seed bounds graphics
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
  (O.option
     O.auto
     (O.long "seed" <> O.help "Give a starting seed" <>
      O.showDefault <>
      O.value 42 <>
      O.short 's' <>
      O.metavar "INT")) <*>
  -- make this a switch:
  (O.option
     O.auto
     (O.long "graphics" <>
      O.help "Choose graphics: [simple | complex]" <>
      O.showDefault <>
      O.value Model.Complex <>
      O.short 'g'))
