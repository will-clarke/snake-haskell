module Attr
  ( defaultMap
  , food
  , snakeBody
  , arena
  , boring
  , title
  ) where

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
    [ (food, V.red `B.on` V.brightBlue)
    , (snakeBody, V.black `B.on` V.brightYellow)
    , (snakeHead, V.brightGreen `B.on` V.brightCyan)
    , (arena, V.white `B.on` V.green)
    , (boring, V.white `B.on` V.black)
    , (title, V.black `B.on` V.white)
    ]

food :: Brick.AttrMap.AttrName
food = Brick.AttrMap.attrName "food"

snakeBody :: Brick.AttrMap.AttrName
snakeBody = Brick.AttrMap.attrName "snakeBody"

-- TODO: Implement this colour!
snakeHead:: Brick.AttrMap.AttrName
snakeHead = Brick.AttrMap.attrName "snakeHead"

arena:: Brick.AttrMap.AttrName
arena = Brick.AttrMap.attrName "arena"

boring:: Brick.AttrMap.AttrName
boring = Brick.AttrMap.attrName "boring"

title :: Brick.AttrMap.AttrName
title = Brick.AttrMap.attrName "title"
