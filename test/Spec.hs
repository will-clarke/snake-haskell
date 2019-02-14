module Main where

import Test.Hspec
import qualified LeaderboardSpec

main :: IO ()
main = hspec LeaderboardSpec.spec
