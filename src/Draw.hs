module Draw
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
import qualified Snake
import qualified Types

drawHeader :: Types.State -> B.Widget Types.Name
drawHeader g =
  Border.borderWithLabel
    (B.str $ Types.title g)
    (B.str (" " ++ show (Types.keyPressed g)) B.<+>
     B.str "   " B.<+>
     getSize B.<+>
     (B.str ("[" ++ show (Types.bounds g) ++ "]")) B.<+>
     B.padLeft B.Max (B.str ("Score: " ++ show (Types.score g) ++ " ")))


-- Example of how to find the screen size... :|
getSize :: B.Widget Types.Name
getSize =
  B.Widget B.Fixed B.Fixed $ do
    c <- B.getContext
    B.render $ B.str $ show (c ^. B.availWidthL, c ^. B.availHeightL)

drawGameUI :: Types.State -> B.Widget Types.Name
drawGameUI state =
  (Center.center $ Border.border $ B.str (realDrawGame state)) B.<=>
  B.str (show $ Types.snake state)

realDrawGame :: Types.State -> String
realDrawGame state =
  unlines $ foldl addCoordsAndIconToString defaultGrid thingsToDraw
  where
    drawableTuples :: Snake.Drawable a => a -> ([Types.Coordinate], Char)
    drawableTuples a = (Snake.coords a, Snake.icon a)
  -- We are having to do this stupid tuple as we can't just create a polymorphic list of snakes & food
    thingsToDraw :: [([Types.Coordinate], Char)]
    thingsToDraw =
      [drawableTuples $ Types.snake state, drawableTuples $ Types.food state]
    bounds = Types.bounds state
    defaultGrid = emptyGrid bounds

addCoordsAndIconToString :: [String] -> ([Types.Coordinate], Char) -> [String]
addCoordsAndIconToString previousString (coords, char) =
   foldr (updateString char) previousString coords

updateString :: Char -> Types.Coordinate -> [String] -> [String]
updateString icon (Types.Coordinate y x) =
  replace2D (const icon) (x, y)

emptyGrid :: Types.Bounds -> [String]
emptyGrid (Types.Bounds width height) =
  let row = replicate width ' '
  in replicate height row

draw :: Types.State -> [B.Widget Types.Name]
draw g =
  [B.withBorderStyle BorderStyle.unicodeRounded $ drawHeader g B.<=> drawGameUI g]

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
