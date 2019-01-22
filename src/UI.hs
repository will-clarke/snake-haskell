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
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((^.))
import qualified Types

drawHeader :: Types.State -> B.Widget()
drawHeader g =
  Border.borderWithLabel
    (B.str $ Types.title g)
    (B.str (" " ++ show  (Types.keyPressed g)) B.<+> B.str "   " B.<+> getSize B.<+>
     B.padLeft B.Max (B.str ("Score: " ++ show (Types.score g) ++ " ")))


-- Example of how to find the screen size... :|
getSize :: B.Widget()
getSize =
  B.Widget B.Fixed B.Fixed $ do
    c <- B.getContext
    B.render $ B.str $ show ((c ^. B.availWidthL), (c ^. B.availHeightL))

drawGame :: Types.State -> B.Widget()
drawGame g =
  Border.border $
  Center.center (B.str $ coolDisplayThing g)
  B.<=> getSize
  B.<=> B.str (show $ Types.snake g)
  where
    coolDisplayThing game = replicate (Types.oscillatingN game) '#'

draw :: Types.State -> [B.Widget()]
draw g =
  [B.withBorderStyle BorderStyle.unicodeRounded $ drawHeader g B.<=> drawGame g]

emptyAttrMap :: a -> B.AttrMap
emptyAttrMap = const (B.attrMap V.currentAttr [])

defaultVty :: IO V.Vty
defaultVty = V.mkVty V.defaultConfig
