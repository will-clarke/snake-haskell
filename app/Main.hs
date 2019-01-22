module Main where

import qualified Brick        as B
import qualified Game.Process as Process
import qualified Types
import qualified UI

app :: B.App Types.State () Types.Name
app = B.App
  { B.appDraw         = UI.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Process.handleEvent
  , B.appStartEvent   = startEvent
  , B.appAttrMap      = UI.emptyAttrMap
  }

startEvent :: Types.State -> B.EventM Types.Name Types.State
startEvent s = do
  return s

-- omg :: a -> B.EventM()Types.State
-- omg a = return a

main :: IO Types.State
main = B.customMain UI.defaultVty Nothing app Types.exState
