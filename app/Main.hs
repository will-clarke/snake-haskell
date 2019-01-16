module Main where

import qualified Brick                      as B
import qualified Brick.BChan                as BChan
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Graphics.Vty               as V
import qualified Lib                        as Lib

-- | Reflect which keys are being pressed
data KeyPressed = KeyUp | KeyDown | Keyieft | KeyRight | KeyNone deriving Show

-- | Our main game state
data Game = Game {
    title                :: String,
    keyPressed           :: KeyPressed,
    oscillatingN         :: Int,
    oscillatingDirection :: Lib.Direction
}

newGame :: Game
newGame =
  Game
    { title = "Super Funky Title"
    , keyPressed = KeyNone
    , oscillatingN = 0
    , oscillatingDirection = Lib.L
    }

firstLine :: B.Widget ()
firstLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "e", B.str "y"]

secondLine :: B.Widget()
secondLine = foldr (B.<+>) B.emptyWidget [B.str "y", B.str "o"]

thirdLine :: B.Widget()
thirdLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "a", B.str " ", B.str "h", B.str "a"]

stackedLines :: B.Widget()
stackedLines = foldr (B.<=>) B.emptyWidget [firstLine, secondLine, thirdLine]

drawHeader :: Game -> B.Widget()
drawHeader g =
  Border.borderWithLabel
    (B.str $ title g)
    (B.str (show $ keyPressed g) B.<+>
      B.padLeft B.Max (B.str "Lives: 0 --- lol"))

drawGame :: Game -> B.Widget()
drawGame g =
  Border.border $
  Center.center $
  stackedLines B.<=> B.str (show $ keyPressed g) B.<=>
  B.str (show $ oscillatingN g)

drawUI :: Game -> [B.Widget()]
drawUI g =
  [B.withBorderStyle BorderStyle.unicodeRounded $ drawHeader g B.<=> drawGame g]

progress :: Game -> Game
  -- TODO: There must be a better way to update games.. Maybe look into lenses?
  -- o_O
progress g =
  let (n, dir) =
        Lib.oscillatingNumber ((oscillatingN g), (oscillatingDirection g))
   in Game ('x' : title g) (keyPressed g) n dir

handleEvent :: Game -> B.BrickEvent()() -> B.EventM() (B.Next Game)
handleEvent g (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt g
handleEvent g (B.VtyEvent (V.EvKey V.KUp [])) = runStep g KeyUp
handleEvent g (B.VtyEvent (V.EvKey V.KDown [])) = runStep g KeyDown
handleEvent g (B.VtyEvent (V.EvKey V.KLeft [])) = runStep g KeyLeft
handleEvent g (B.VtyEvent (V.EvKey V.KRight [])) = runStep g KeyRight
handleEvent g _ = runStep (progress g) KeyNone

runStep :: Game -> KeyPressed -> B.EventM() (B.Next Game)
runStep g KeyUp    = B.continue $ g { keyPressed = KeyUp }
runStep g KeyDown  = B.continue $ g { keyPressed = KeyDown }
runStep g KeyRight = B.continue $ g { keyPressed = KeyRight }
runStep g KeyLeft  = B.continue $ g { keyPressed = KeyLeft }
runStep g _        = B.continue g

app :: B.App Game()()
app = B.App
  { B.appDraw         = drawUI
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = handleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const (B.attrMap V.currentAttr [])
  }

main :: IO Game
main =
    B.customMain (V.mkVty V.defaultConfig) Nothing app newGame

