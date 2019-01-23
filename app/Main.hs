module Main where

import qualified Brick        as B
import qualified Brick.Main
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
  mExtent <- Brick.Main.lookupExtent Types.FooBox
  return s {Types.bounds = boundsFromExtent mExtent}
  where
    boundsFromExtent extent =
      case extent of
        Nothing -> Types.Bounds {Types.maxWidth = 20, Types.maxHeight = 10}
        Just (B.Extent _ _ (width, height) _) ->
          Types.Bounds {Types.maxWidth = width, Types.maxHeight = height}

-- omg :: a -> B.EventM()Types.State
-- omg a = return a

main :: IO Types.State
main = B.customMain UI.defaultVty Nothing app Types.exState
