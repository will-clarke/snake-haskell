module Main where

import qualified Brick as B
import qualified Game  as Game
import qualified UI as UI
import qualified Lib   as Lib
import qualified Graphics.Vty               as V


app :: B.App Game.State()()
app = B.App
  { B.appDraw         = UI.draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = Game.handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const (B.attrMap V.currentAttr [])
  }

main :: IO Game.State
main =
    B.customMain (V.mkVty V.defaultConfig) Nothing app Game.newState

