module LeaderboardSpec where

import           Control.Exception (evaluate)
import qualified Leaderboard
import qualified Model
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Leaderboard.maybeLineContaining" $ do
    context "when there is a matching line for a given bounds" $ do
      it "matches the line" $ do
        Leaderboard.maybeLineContaining bounds matchingString `shouldBe`
          Just yes
    context "when there is a matching line for a given bounds" $ do
      it "matches the line" $ do
        Leaderboard.maybeLineContaining bounds nonMatchinString `shouldBe`
          Nothing
    context "when there is a blank file" $ do
      it "returns Nothing" $ do
        Leaderboard.maybeLineContaining bounds "" `shouldBe` Nothing

bounds :: Model.Bounds
bounds = Model.Bounds 11 12

yes :: [Char]
yes = "Bounds {maxWidth = 11, maxHeight = 12} - Yep"

no1 :: [Char]
no1 = "Bounds {maxWidth = 12, maxHeight = 12} - Nope"

no2 :: [Char]
no2 = "Bounds {maxWidth = 10, maxHeight = 12} - NOOO"

matchingString :: [Char]
matchingString = no1 ++ "\n" ++ yes ++ "\n" ++ no2

nonMatchinString :: [Char]
nonMatchinString = no1 ++ "\n" ++ no2
