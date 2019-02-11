module Draw
  ( draw,
    emptyAttrMap,
    defaultVty
  ) where

import qualified Brick                      as B
import qualified Brick.AttrMap
import           Brick.BChan                ()
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Graphics.Vty               as V
import qualified Model

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
drawGame _state = foldl (B.<=>) B.emptyWidget lines
  where lines = [B.str ("hey") B.<=> B.str ("wassup")]
  -- -- can we improve this? reverse $ foldl.. would foldr work?
  -- foldl addCoordsAndIconToString defaultGrid thingsToDraw
  -- where
  --   drawableTuples :: Typeclasses.Drawable a => a -> ([Model.Coordinate], Char)
  -- -- todo: this will break things. We want to sort this out. We're currently in the process of making snakes draw & food drawing return a widget rather than a char.
  --   drawableTuples a = (Typeclasses.coords a, Typeclasses.icon a)
  -- -- We are having to do this stupid tuple as we can't just create a polymorphic list of snakes & food
  --   thingsToDraw :: [([Model.Coordinate], Char)]
  --   thingsToDraw =
  --     [drawableTuples $ Model.snake state, drawableTuples $ Model.food state]
  --   bounds = Model.bounds state
  --   defaultGrid = emptyGrid bounds

-- widgetRows

addCoordsAndIconToString :: [String] -> ([Model.Coordinate], Char) -> [String]
addCoordsAndIconToString previousString (coords, char) =
   foldr (updateString char) previousString coords

updateString :: Char -> Model.Coordinate -> [String] -> [String]
updateString icon (Model.Coordinate y x) =
  replace2D (const icon) (x, y)

emptyGrid :: Model.Bounds -> [String]
emptyGrid (Model.Bounds width height) =
  let row = replicate width ' '
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

