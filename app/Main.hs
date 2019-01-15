module Main where

import           Brick ((<+>), (<=>), Widget, emptyWidget, str, simpleMain, withBorderStyle)
import           Brick.Widgets.Border (borderWithLabel)
import           Brick.Widgets.Border.Style (unicodeRounded)
import           Brick.Widgets.Center (center)

firstLine :: Widget()
firstLine = foldr (<+>) emptyWidget [str "h", str "e", str "y"]

secondLine :: Widget()
secondLine = foldr (<+>) emptyWidget [str "y", str "o"]

thirdLine :: Widget()
thirdLine = foldr (<+>) emptyWidget [str "h", str "a", str " ", str "h", str "a"]

stackedLines :: Widget()
stackedLines = foldr (<=>) emptyWidget [firstLine, secondLine, thirdLine]

ui :: Widget ()
ui =
  withBorderStyle unicodeRounded $
  borderWithLabel
    (str "Hello!")
    (center stackedLines)

main :: IO ()
main = simpleMain ui
