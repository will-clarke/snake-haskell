module Main where

import qualified Brick        as B
import qualified Game.Process as Process
import qualified Game.State   as S
import qualified UI

app :: B.App S.State()()
app = B.App
  { B.appDraw         = UI.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Process.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = UI.emptyAttrMap
  }

main :: IO S.State
main =
    B.customMain UI.defaultVty Nothing app S.newState

