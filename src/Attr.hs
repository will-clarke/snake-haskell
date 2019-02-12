module Attr (defaultMap, food, snakeBody) where

import qualified Brick                   as B
import qualified Brick.AttrMap
import qualified Brick.Util
import qualified Graphics.Vty            as V
import qualified Graphics.Vty.Attributes as Attrs

globalDefault :: Attrs.Attr
globalDefault = V.white `Brick.Util.on` V.blue

defaultMap :: Brick.AttrMap.AttrMap
defaultMap =
  Brick.AttrMap.attrMap
    globalDefault
    [ (food, V.red `B.on` V.brightMagenta)
    , (snakeBody, V.black `B.on` V.yellow)
    , (snakeHead, V.brightGreen `B.on` V.brightCyan)
    , (Brick.AttrMap.attrName "foundFull", V.white `B.on` V.green)
    , (Brick.AttrMap.attrName "foundFgOnly", B.fg V.red)
    , (Brick.AttrMap.attrName "general", V.yellow `B.on` V.black)
    , ( Brick.AttrMap.attrName "general" <> Brick.AttrMap.attrName "specific"
      , B.fg V.cyan)
    -- , (Brick.AttrMap.attrName "linked", B.fg V.yellow `V.withURL` "http://www.google.com/")
    ]

food :: Brick.AttrMap.AttrName
food = Brick.AttrMap.attrName "food"

snakeBody :: Brick.AttrMap.AttrName
snakeBody = Brick.AttrMap.attrName "snakeBody"

snakeHead:: Brick.AttrMap.AttrName
snakeHead = Brick.AttrMap.attrName "snakeHead"
