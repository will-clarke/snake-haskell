module Main where

import qualified Brick as B
import qualified Brick.BChan as BChan
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Graphics.Vty as V

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

firstLine :: B.Widget Name
firstLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "e", B.str "y"]

secondLine :: B.Widget Name
secondLine = foldr (B.<+>) B.emptyWidget [B.str "y", B.str "o"]

thirdLine :: B.Widget Name
thirdLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "a", B.str " ", B.str "h", B.str "a"]

stackedLines :: B.Widget Name
stackedLines = foldr (B.<=>) B.emptyWidget [firstLine, secondLine, thirdLine]

data KeyPressed = KeyUp | KeyDown | KeyLeft | KeyRight | KeyNone deriving Show

data Game = Game {
    theText :: String,
    keyPressed :: KeyPressed
}

drawUI :: Game -> [B.Widget Name]
drawUI g =
  [B.withBorderStyle BorderStyle.unicodeRounded $
  Border.borderWithLabel
    (B.str $ theText g)
    (Center.center (stackedLines B.<=> (B.str $ show $ keyPressed g)))]

progress :: Game -> Game
progress g = Game ( 'x' : (theText g) ) (keyPressed g)

handleEvent :: Game -> B.BrickEvent Name Tick -> B.EventM Name (B.Next Game)
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey V.KUp [])) = runStep g KeyUp
handleEvent g (B.VtyEvent (V.EvKey V.KDown [])) = runStep g KeyDown
handleEvent g (B.VtyEvent (V.EvKey V.KLeft [])) = runStep g KeyLeft
handleEvent g (B.VtyEvent (V.EvKey V.KRight [])) = runStep g KeyRight
handleEvent g _ = runStep (progress g) KeyNone

runStep :: Game -> KeyPressed -> B.EventM Name (B.Next Game)
runStep g KeyUp = B.continue $ g { keyPressed = KeyUp }
runStep g KeyDown = B.continue $ g { keyPressed = KeyDown }
runStep g KeyRight = B.continue $ g { keyPressed = KeyRight }
runStep g KeyLeft = B.continue $ g { keyPressed = KeyLeft }
runStep g _ = B.continue $ g

app :: B.App Game Tick Name
app = B.App
  { B.appDraw         = drawUI
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const (B.attrMap (V.currentAttr) [])
  }

main :: IO Game
main = do
    chan <- BChan.newBChan 10
    B.customMain (V.mkVty V.defaultConfig) (Just chan) app $ (Game "text" KeyNone)

