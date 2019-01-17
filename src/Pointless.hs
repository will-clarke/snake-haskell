module Pointless
    (
      oscillatingNumber,
      Direction(..),
    ) where

data Direction
  = L
  | R

oscillationLenght :: Int
oscillationLenght = 15

oscillationPadding :: Int
oscillationPadding = 5

type Oscillation = (Int, Direction)

incr :: Int -> Int
incr n = n + 1

decr :: Int -> Int
decr n = n - 1

oscillatingNumber :: Oscillation -> Oscillation
oscillatingNumber (n, R)
  | n < (oscillationLenght + oscillationPadding) = (incr n, R)
  | otherwise = (decr n, L)
oscillatingNumber (n, L)
  | n > (0 + oscillationPadding) = (decr n, L)
  | otherwise = (incr n, R)
