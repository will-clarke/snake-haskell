module Main where

import qualified Brick       as B
import qualified Brick.BChan
import           Brick.Main  ()
import qualified Draw
import qualified State
import qualified Model
import qualified Update
import qualified Control.Monad
import qualified Control.Concurrent

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
  chan <- Brick.BChan.newBChan 10
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Model.Tick
      Control.Concurrent.threadDelay delay
  B.customMain Draw.defaultVty (Just chan) app State.exState
