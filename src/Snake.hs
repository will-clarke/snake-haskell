module Snake
  ( initialSnake
  , moveSnake
  ) where

import qualified Food
import qualified Model as M

initialSnake :: M.Bounds -> M.Snake
initialSnake bounds =
  let maxWidth = M.maxWidth bounds
      maxHeight = M.maxHeight bounds
      midX = maxWidth `div` 2
      midY = maxHeight `div` 2
   in M.Snake
        { M.getSegments =
            [ M.Coordinate {M.x = midX + 1, M.y = midY}
            , M.Coordinate {M.x = midX, M.y = midY}
            , M.Coordinate {M.x = midX - 1, M.y = midY}
            ]
        }

moveSnake :: M.Game -> M.Snake
moveSnake game =
  let snake = M.snake game
      food = M.food game
      snakeCoords = M.getSegments snake
      direction = M.direction game
      previousDirection = M.previousDirection game
      isEating = Food.snakeEating snake food
      restOfSnake =
        if isEating
          then snakeCoords
          else init snakeCoords
   in M.Snake (nextHeadCoords (head snakeCoords) direction previousDirection : restOfSnake)

-- nextHeadCoords :: M.Coordinate -> M.Direction -> M.Direction -> M.Coordinate
-- nextHeadCoords M.Coordinate{M.x = x, M.y = y} M.North M.South = M.Coordinate{M.x = 10, M.y = 10}
-- nextHeadCoords M.Coordinate{M.x = x, M.y = y} M.North _ = M.Coordinate{M.x = x, M.y = y + 1}
-- -- nextHeadCoords coords M.South M.North = coords
-- nextHeadCoords M.Coordinate{M.x = x, M.y = y} M.South _ = M.Coordinate{M.x = x, M.y = y - 1}
-- -- nextHeadCoords coords M.West M.East = coords
-- nextHeadCoords M.Coordinate{M.x = x, M.y = y} M.West _ = M.Coordinate{M.x = x - 1, M.y = y}
-- -- nextHeadCoords coords M.East M.West = coords
-- nextHeadCoords M.Coordinate{M.x = x, M.y = y} M.East _ = M.Coordinate{M.x = x + 1, M.y = y}

nextHeadCoords :: M.Coordinate -> M.Direction -> M.Direction -> M.Coordinate
nextHeadCoords c  M.North M.South = moveSouth c
nextHeadCoords c  M.North _       = moveNorth c
nextHeadCoords c  M.East M.West   = moveWest c
nextHeadCoords c  M.East _        = moveEast c
nextHeadCoords c  M.South M.North = moveNorth c
nextHeadCoords c  M.South _       = moveSouth c
nextHeadCoords c  M.West M.East   = moveEast c
nextHeadCoords c  M.West _        = moveWest c

moveSouth :: M.Coordinate -> M.Coordinate
moveSouth (M.Coordinate x y) = M.Coordinate x (y - 1)

moveNorth :: M.Coordinate -> M.Coordinate
moveNorth (M.Coordinate x y) = M.Coordinate x (y + 1)

moveEast :: M.Coordinate -> M.Coordinate
moveEast (M.Coordinate x y) = M.Coordinate (x + 1) y

moveWest :: M.Coordinate -> M.Coordinate
moveWest (M.Coordinate x y) = M.Coordinate (x - 1) y
