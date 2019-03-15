module Draw
  ( draw
  , emptyAttrMap
  , defaultVty
  , gameOverWidget
  ) where

import qualified Attr
import qualified Brick                      as B
import qualified Brick.AttrMap
import           Brick.BChan                ()
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center       as Center
import qualified Brick.Widgets.Core
import qualified Data.Map
import qualified Graphics.Vty               as V
import qualified Leaderboard
import qualified Model
import qualified Typeclasses

drawHeader :: Model.Game -> B.Widget Model.Name
drawHeader game =
  Border.borderWithLabel
    (B.withAttr Attr.title $ B.str $ Model.getTitle game)
    (B.withAttr Attr.arena $B.str (show (Model.direction game)) B.<+>
     B.padLeft B.Max (B.str ("Score: " ++ show (Model.getScore game) ++ " ")))

drawGameWidget :: Model.Game -> B.Widget Model.Name
drawGameWidget game =
  foldl
    (B.<=>)
    B.emptyWidget
    (reverse $ map (foldr (B.<+>) B.emptyWidget) $ widgetRows game)

widgetRows :: Model.Game -> [[B.Widget Model.Name]]
widgetRows game = foldr updateWidgets defaultGrid $ drawableEntities game
  where bounds = Model.getBounds game
        defaultGrid = emptyGrid bounds

drawableEntities :: Model.Game -> [CoordWidget]
drawableEntities game = [toCoordWidgets snake, toCoordWidgets food]
  where
    snake = Model.getSnake game
    food = Model.getFood game

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

drawGame :: Model.Game -> [B.Widget Model.Name]
drawGame game =
  let getGraphics = Model.getGraphics game
      gameWidget = drawHeader game B.<=> Center.center (Border.border $ drawGameWidget game)
   in case getGraphics of
        Model.Simple -> [B.withBorderStyle BorderStyle.ascii $ B.forceAttr Attr.boring gameWidget]
        Model.Complex -> [B.withBorderStyle BorderStyle.unicodeRounded gameWidget]

draw :: Model.State -> [B.Widget Model.Name]
draw (Model.Playing game) = drawGame game
draw (Model.Replaying game) = drawGame game
draw (Model.StartScreen options _tvar) =
  let style =
        case Model.getStartGraphics options of
          Model.Simple  -> BorderStyle.ascii
          Model.Complex -> BorderStyle.unicodeRounded
   in [ B.withBorderStyle style $
        Center.center (Border.border $ B.str "Welcome")
      ]


draw (Model.Paused _game) = [Center.center (Border.border $ B.str "** PAUSED **")]
draw (Model.GameOver game) =
  [ Center.center $
    B.str
      ("\n\
    \    YOU DIED\n\n\
    \    /     \\ \n\
    \   | () () |\n\
    \    \\  ^  /\n\
    \     |||||\n\
    \     |||||\n\n\
    \Press <Spacebar> to continue\n" ++
       replayNote)
  ]
  where
    replayNote =
      if null (Model.getPreviousGames game)
        then ""
        else "\n\
    \Press <r> to View a replay"


gameOverWidget :: Model.Attempt -> Model.Leaderboard -> B.Widget Model.Name
gameOverWidget attempt@(Model.Attempt league getScore) leaderboard =
  let newHighScore = Leaderboard.isHighScore attempt leaderboard
      defaultScore = Model.Score 0
      previousHighScore =
        Data.Map.findWithDefault
          defaultScore
          league
          (Model.getLeaguesAndScores leaderboard)
      getTitle =
        if newHighScore
          then "NEW HIGH GETSCORE -- OMG CONGRATS <3"
          else "You can do better. I BELIEVE IN YOU"
      lastBit =
        if newHighScore
          then ""
          else "\n\n\nPrevious high getScore was " ++
               show (Model.getPoints previousHighScore)
      widgetText =
        getTitle ++
        "\n\nYou got " ++
        show (Model.getPoints getScore) ++ " points" ++ lastBit
      widget' = B.str widgetText
      paddedWidget = Brick.Widgets.Core.padAll 3 widget'
      boarderedWidget = Border.border paddedWidget
   in Center.center boarderedWidget

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

