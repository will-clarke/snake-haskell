module Main where

import qualified Brick       as B
import qualified Brick.BChan
import           Brick.Main  ()
import qualified Draw
import qualified State
import qualified Types
import qualified Update
import qualified Control.Monad
import qualified Control.Concurrent

app :: B.App Types.State Types.Tick Types.Name

app = B.App
  { B.appDraw         = Draw.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Update.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = Draw.emptyAttrMap
  }

main :: IO Types.State
main = do
  let delay = 100000
  chan <- Brick.BChan.newBChan 10
  Control.Monad.void . Control.Concurrent.forkIO $
    Control.Monad.forever $ do
      Brick.BChan.writeBChan chan Types.Tick
      Control.Concurrent.threadDelay delay
  B.customMain Draw.defaultVty (Just chan) app State.exState
