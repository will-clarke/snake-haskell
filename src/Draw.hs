module Draw
  ( draw
  , emptyAttrMap
  , defaultVty
  ) where

import qualified Attr
import qualified Brick                      as B
import qualified Brick.AttrMap
import           Brick.BChan                ()
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Graphics.Vty               as V
import qualified Model
import qualified Typeclasses

drawHeader :: Model.Game -> B.Widget Model.Name
drawHeader game =
  Border.borderWithLabel
    (B.withAttr foundFgOnly $ B.str $ Model.title game)
    (B.withAttr foundFull $B.str (show (Model.direction game)) B.<+>
     B.padLeft B.Max (B.str ("Score: " ++ show (Model.score game) ++ " ")))
  where foundFull = Brick.AttrMap.attrName "foundFull"
        foundFgOnly = Brick.AttrMap.attrName "foundFgOnly"
        -- general = Brick.AttrMap.attrName "general"

-- Example of how to find the screen size... :|
-- getSize :: B.Widget Model.Name
-- getSize =
--   B.Widget B.Fixed B.Fixed $ do
--     c <- B.getContext
--     B.render $ B.str $ show (c ^. B.availWidthL, c ^. B.availHeightL)

drawGame :: Model.Game -> B.Widget Model.Name
drawGame game =
  foldl
    (B.<=>)
    B.emptyWidget
    (reverse $ map (foldr (B.<+>) B.emptyWidget) $ widgetRows game)

widgetRows :: Model.Game -> [[B.Widget Model.Name]]
widgetRows game = foldr updateWidgets defaultGrid $ drawableEntities game
  where bounds = Model.bounds game
        defaultGrid = emptyGrid bounds

drawableEntities :: Model.Game -> [CoordWidget]
drawableEntities game = [toCoordWidgets snake, toCoordWidgets food]
  where
    snake = Model.snake game
    food = Model.food game

-- CoordWidget is an abstract implementation of `Drawable`s
data CoordWidget = CoordWidget
  { coords :: [Model.Coordinate]
  , widget :: B.Widget Model.Name
  }

toCoordWidgets :: Typeclasses.Drawable d => d -> CoordWidget
toCoordWidgets d =
  CoordWidget {coords = Typeclasses.coords d, widget = Typeclasses.widget d}

updateWidgets :: CoordWidget -> [[B.Widget Model.Name]] -> [[B.Widget Model.Name]]
updateWidgets (CoordWidget coords_ widget_) previousWidgets =
  foldr
    (updateWidget widget_)
    previousWidgets
    coords_

updateWidget :: B.Widget Model.Name -> Model.Coordinate -> [[B.Widget Model.Name]] -> [[B.Widget Model.Name]]
updateWidget widget_ (Model.Coordinate y x) = replace2D (const widget_) (x, y)

emptyGrid :: Model.Bounds -> [[B.Widget Model.Name]]
emptyGrid (Model.Bounds width height) =
  let row = replicate width (B.withAttr Attr.arena $ B.str " ")
  in replicate height row

draw :: Model.State -> [B.Widget Model.Name]
draw (Model.Playing game) =
  let graphics = Model.graphics game
      gameWidget = drawHeader game B.<=> Center.center (Border.border $ drawGame game)
   in case graphics of
        Model.Simple -> [B.withBorderStyle BorderStyle.ascii $ B.forceAttr Attr.boring gameWidget]
        Model.Complex -> [B.withBorderStyle BorderStyle.unicodeRounded gameWidget]
draw (Model.StartScreen _options) = [Center.center (Border.border $ B.str "Welcome")]
draw (Model.Paused _game) = [Center.center (Border.border $ B.str "** PAUSED **")]
draw (Model.GameOver score) =
  [Center.center (Border.border $ B.str $ "You lost ;(\n\n score = " ++ show (Model.getPoints $ Model.getScore score))]

emptyAttrMap :: a -> B.AttrMap
emptyAttrMap = const (B.attrMap V.currentAttr [])

defaultVty :: IO V.Vty
defaultVty = V.mkVty V.defaultConfig

-- this code is just for updating the list
-- shamelessly stolen from https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
-- It's kind of sneaky
replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = f x : xs
replace f n (x:xs) = x : replace f (n - 1) xs
replace _ _ []     = []

replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x, y) = replace (replace f y) x

