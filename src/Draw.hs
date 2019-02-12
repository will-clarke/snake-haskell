module Draw
  ( draw
  , emptyAttrMap
  , defaultVty
  ) where

import qualified Brick                      as B
import qualified Brick.AttrMap
import           Brick.BChan                ()
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Graphics.Vty               as V
import qualified Model
import qualified Typeclasses

drawHeader :: Model.State -> B.Widget Model.Name
drawHeader state =
  Border.borderWithLabel
    (B.withAttr foundFgOnly $ B.str $ Model.title state)
    (B.withAttr foundFull $B.str (show (Model.direction state, Model.previousDirection state)) B.<+>
     B.padLeft B.Max (B.str ("Score: " ++ show (Model.score state) ++ " ")))
  where foundFull = Brick.AttrMap.attrName "foundFull"
        foundFgOnly = Brick.AttrMap.attrName "foundFgOnly"
        -- general = Brick.AttrMap.attrName "general"

-- Example of how to find the screen size... :|
-- getSize :: B.Widget Model.Name
-- getSize =
--   B.Widget B.Fixed B.Fixed $ do
--     c <- B.getContext
--     B.render $ B.str $ show (c ^. B.availWidthL, c ^. B.availHeightL)

drawGame :: Model.State -> B.Widget Model.Name
-- drawGame state = foldl (B.<=>) B.emptyWidget (foldl (B.<+>) B.emptyWidget $ widgetRows state)

drawGame state =
  foldl
    (B.<=>)
    B.emptyWidget
    (map (foldl (B.<+>) B.emptyWidget) $ widgetRows state)
  -- -- can we improve this? reverse $ foldl.. would foldr work?
  -- foldl addCoordWidgetToString defaultGrid thingsToDraw
  -- where
  --   drawableTuples :: Typeclasses.Drawable a => a -> ([Model.Coordinate], Char)
  -- -- todo: this will break things. We want to sort this out. We're currently in the process of making snakes draw & food drawing return a widget rather than a char.
  --   drawableTuples a = (Typeclasses.coords a, Typeclasses.widget a)
  -- -- We are having to do this stupid tuple as we can't just create a polymorphic list of snakes & food
  --   thingsToDraw :: [([Model.Coordinate], Char)]
  --   thingsToDraw =
  --     [drawableTuples $ Model.snake state, drawableTuples $ Model.food state]
  --   bounds = Model.bounds state
  --   defaultGrid = emptyGrid bounds

widgetRows :: Model.State -> [[B.Widget Model.Name]]
widgetRows state = foldr updateWidgets defaultGrid $ drawableEntities state
  where bounds = Model.bounds state
        defaultGrid = emptyGrid bounds

drawableEntities :: Model.State -> [CoordWidget]
drawableEntities state = [toCoordWidgets snake, toCoordWidgets food]
  where
    snake = Model.snake state
    food = Model.food state

-- CoordWidget is an abstract implementation of `Drawable`s
data CoordWidget = CoordWidget
  { coords :: [Model.Coordinate]
  , widget :: B.Widget Model.Name
  }

toCoordWidgets :: Typeclasses.Drawable d => d -> CoordWidget
toCoordWidgets d =
  CoordWidget {coords = Typeclasses.coords d, widget = Typeclasses.widget d}

-- addCoordWidgetToString :: [String] -> ([Model.Coordinate], Char) -> [String]
-- addCoordWidgetToString previousString (coords, char) =
--    foldr (updateWidgets char) previousString coords

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
  let row = replicate width (B.str " ")
  in replicate height row

draw :: Model.State -> [B.Widget Model.Name]
draw state =
  [ B.withBorderStyle BorderStyle.unicodeRounded $
    drawHeader state B.<=> Center.center (Border.border $ drawGame state)
  ]

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

