module Main where

import qualified Brick as B
import qualified Game  as Game
import qualified Lib   as Lib
import qualified UI    as UI

app :: B.App Game.State()()
app = B.App
  { B.appDraw         = UI.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Game.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = UI.emptyAttrMap
  }

main :: IO Game.State
main =
    B.customMain UI.defaultVty Nothing app Game.newState

