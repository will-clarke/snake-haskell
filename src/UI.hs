module UI
  ( draw,
    emptyAttrMap,
    defaultVty
  ) where

import qualified Brick                      as B
import           Brick.BChan                ()
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Types
import qualified Graphics.Vty               as V
import Lens.Micro ((^.))



firstLine :: B.Widget ()
firstLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "e", B.str "y"]

secondLine :: B.Widget()
secondLine = foldr (B.<+>) B.emptyWidget [B.str "y", B.str "o"]

thirdLine :: B.Widget()
thirdLine = foldr (B.<+>) B.emptyWidget [B.str "h", B.str "a", B.str " ", B.str "h", B.str "a"]

stackedLines :: B.Widget()
stackedLines = foldr (B.<=>) B.emptyWidget [firstLine, secondLine, thirdLine]

drawHeader :: Types.State -> B.Widget()
drawHeader g =
  Border.borderWithLabel
    (B.str $ Types.title g)
    (B.str (show $ Types.keyPressed g) B.<+>
    (B.str "   ") B.<+>
    getSize B.<+>
      B.padLeft B.Max (B.str "Lives: 0 --- lol"))


-- Example of how to find the screen size... :|
getSize :: B.Widget()
getSize =
  B.Widget B.Fixed B.Fixed $ do
    c <- B.getContext
    B.render $ B.str $ show ((c ^. B.availWidthL), (c ^. B.availHeightL))

drawGame :: Types.State -> B.Widget()
drawGame g =
  Border.border $
  Center.center stackedLines B.<=> Center.center (B.str $ coolDisplayThing g) B.<=> getSize
  where
    coolDisplayThing game = replicate (Types.oscillatingN game) '#'

draw :: Types.State -> [B.Widget()]
draw g =
  [B.withBorderStyle BorderStyle.unicodeRounded $ drawHeader g B.<=> drawGame g]

emptyAttrMap :: a -> B.AttrMap
emptyAttrMap = const (B.attrMap V.currentAttr [])

defaultVty :: IO V.Vty
defaultVty = V.mkVty V.defaultConfig
