module Main where

import qualified Brick                   as B
import qualified Brick.AttrMap
import qualified Brick.BChan
import           Brick.Main              ()
import qualified Control.Concurrent
import qualified Control.Monad
import qualified Data.Semigroup
import qualified Draw
import qualified Model
import qualified Options.Applicative     as O
import qualified State
import qualified Update
import qualified Attr

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
  (Options seed) <- O.execParser fullopts
  chan <- Brick.BChan.newBChan 10
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Model.Tick
      Control.Concurrent.threadDelay delay
  B.customMain Draw.defaultVty (Just chan) app $ startingState seed

startingState :: Int -> Model.State
startingState seed = State.initialState seed bounds
  where
    bounds = Model.Bounds {Model.maxHeight = 20, Model.maxWidth = 60}

fullopts :: O.ParserInfo Options
fullopts = O.info (O.helper <*> parsedOptions)
  (  O.fullDesc
  <> O.header "Snake - majestically displayed in ASCII glory" )

data Options = Options { startSeed :: Int } deriving Show

parsedOptions :: O.Parser Options
parsedOptions = Options <$> O.option O.auto
  (O.long "seed"
   Data.Semigroup.<> O.help "Give a starting seed"
   Data.Semigroup.<> O.showDefault
  -- Maybe this default seed should be random? And we could extract it and display it??
   Data.Semigroup.<> O.value 42
   Data.Semigroup.<> O.short 's'
   Data.Semigroup.<> O.metavar "INT"
  )
