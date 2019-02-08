module Main where

import qualified Brick                   as B
import qualified Brick.AttrMap
import qualified Brick.BChan
import           Brick.Main              ()
import qualified Brick.Util
import qualified Control.Concurrent
import qualified Control.Monad
import qualified Data.Semigroup
import qualified Draw
import qualified Graphics.Vty            as V
import qualified Graphics.Vty.Attributes as Attrs
import qualified Model
import qualified Options.Applicative     as O
import qualified State
import qualified Update

app :: B.App Model.State Model.Tick Model.Name

app = B.App
  { B.appDraw         = Draw.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Update.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = Draw.emptyAttrMap
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


globalDefault :: Attrs.Attr
globalDefault = V.white `Brick.Util.on` V.blue

-- /Users/wmmc/.stack/indices/Hackage/packages/vty/5.25.1/vty-5.25.1/src/Graphics/Vty/Attributes.hs
theMap :: Brick.AttrMap.AttrMap
theMap =
  Brick.AttrMap.attrMap
    globalDefault
    [ (Brick.AttrMap.attrName "foundFull", V.white `B.on` V.green)
    , (Brick.AttrMap.attrName "foundFgOnly", B.fg V.red)
    , (Brick.AttrMap.attrName "general", V.yellow `B.on` V.black)
    , (Brick.AttrMap.attrName "general" <> Brick.AttrMap.attrName "specific", B.fg V.cyan)
    -- , (Brick.AttrMap.attrName "linked", B.fg V.yellow `V.withURL` "http://www.google.com/")
    ]
