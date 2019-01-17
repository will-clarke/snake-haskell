module UI
  ( draw,
    emptyAttrMap,
    defaultVty
  ) where

import qualified Brick                      as B
import Brick.BChan()
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Game
import qualified Graphics.Vty               as V



firstLine :: B.Widget ()
firstLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "e", B.str "y"]

secondLine :: B.Widget()
secondLine = foldr (B.<+>) B.emptyWidget [B.str "y", B.str "o"]

thirdLine :: B.Widget()
thirdLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "a", B.str " ", B.str "h", B.str "a"]

stackedLines :: B.Widget()
stackedLines = foldr (B.<=>) B.emptyWidget [firstLine, secondLine, thirdLine]

drawHeader :: Game.State -> B.Widget()
drawHeader g =
  Border.borderWithLabel
    (B.str $ Game.title g)
    (B.str (show $ Game.keyPressed g) B.<+>
      B.padLeft B.Max (B.str "Lives: 0 --- lol"))

drawGame :: Game.State -> B.Widget()
drawGame g =
  Border.border $
  Center.center stackedLines B.<=> Center.center (B.str $ coolDisplayThing g)
  where
    coolDisplayThing game = replicate (Game.oscillatingN game) '#'

draw :: Game.State -> [B.Widget()]
draw g =
  [B.withBorderStyle BorderStyle.unicodeRounded $ drawHeader g B.<=> drawGame g]

emptyAttrMap :: a -> B.AttrMap
emptyAttrMap = const (B.attrMap V.currentAttr [])

defaultVty :: IO V.Vty
defaultVty = V.mkVty V.defaultConfig
