module LeaderboardSpec where

import           Control.Exception (evaluate)
import qualified Control.Monad
import qualified Data.List
import qualified Data.Map
import qualified Leaderboard
import qualified Model
import           Test.Hspec
import           Test.QuickCheck

-- exampleString :: Gen StringSample
-- exampleString = frequency [(1, arbitrary) (2, arbitrary)]

-- newtype StringSample = StringSample String

-- instance Arbitrary StringSample where
--   -- arbitrary :: Gen StringSample
--   arbitrary = frequency [
--     (1, return $ StringSample "hey"),
--     (1, return $ StringSample $ String arbitrary)
--                         ]

instance Arbitrary Model.Leaderboard where
  arbitrary = Model.Leaderboard <$> arbitrary

instance Arbitrary Model.Bounds where
  arbitrary = Model.Bounds <$> arbitrary <*> arbitrary

instance Arbitrary Model.League where
  arbitrary = Model.League <$> exampleBounds

instance Arbitrary Model.Score where
  arbitrary = Model.Score <$> arbitrary

instance Arbitrary Model.Attempt where
  arbitrary = Model.Attempt <$> arbitrary <*> arbitrary

exampleBounds :: Gen Model.Bounds
exampleBounds = Model.Bounds <$> arbitrary <*> arbitrary

exampleLeague :: Gen Model.League
exampleLeague = Model.League <$> exampleBounds

exampleScore :: Gen Model.Score
exampleScore = Model.Score <$> arbitrary

exampleLeaderboard :: Gen Model.Leaderboard
exampleLeaderboard = Model.Leaderboard <$> arbitrary


spec :: Spec
spec = do
  describe "Leaderboard.serialiseLeague" $ do
    it "produces a new League string" $ do
      Leaderboard.serialiseLeague (Model.League $ Model.Bounds 9 3) `shouldBe`
        "[League - Width:9,Height:3]"
  describe "Leaderboard.deserialiseLeague" $ do
    it "constructs a new League" $ do
      Leaderboard.deserialiseLeague "[League - Width:9,Height:3]" `shouldBe`
        (Just $ Model.League $ Model.Bounds 9 3)
  describe "League" $ do
    it "should be able to serialise and deserialise properly" $ do
      forAll exampleLeague $ \league ->
        (Leaderboard.deserialiseLeague (Leaderboard.serialiseLeague league)) `shouldBe`
        Just league
  describe "Attempts" $ do
    it "should be deserialisable" $ do
      Leaderboard.deserialiseAttempt
        "[League - Width:10,Height:5] -- [Score - Points:100]\n" `shouldBe`
        Just
          (Model.Attempt (Model.League $ Model.Bounds 10 5) (Model.Score 100))
  describe "entire Leaderboard file" $ do
    it "should be deserialisable" $ do
      Leaderboard.deserialiseLeaderboard
        "[League - Width:10,Height:5] -- [Score - Points:100]\n\
      \[League - Width:11,Height:5] -- [Score - Points:101]\n\
      \[League - Width:12,Height:5] -- [Score - Points:102]" `shouldBe`
        Just
          (Model.Leaderboard
             (Data.Map.fromList
                [ (Model.League (Model.Bounds 10 5), (Model.Score 100))
                , (Model.League (Model.Bounds 11 5), (Model.Score 101))
                , (Model.League (Model.Bounds 12 5), (Model.Score 102))
                ]))


-- Data.Map.empty)
      -- Leaderboard.deserialiseLeaderboard "[League - Width:10,Height:5] -- [Score - Points:100]\
      -- \[League - Width:11,Height:5] -- [Score - Points:101]\
      -- \[League - Width:12,Height:5] -- [Score - Points:102]" `shouldBe` Just (Model.Leaderboard Data.Map.empty)
    -- it "should be serialisable" $ do
       -- Leaderboard.serialiseLeaderboard (Model.Leaderboard {Model.getLeagues = Data.Map.fromList [(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -26, Model.maxHeight = 3}},Model.Score {Model.getPoints = 3}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -25, Model.maxHeight = -13}},Model.Score {Model.getPoints = -23}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -19, Model.maxHeight = 22}},Model.Score {Model.getPoints = 4}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -18, Model.maxHeight = 11}},Model.Score {Model.getPoints = 13}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -15, Model.maxHeight = 4}},Model.Score {Model.getPoints = 26}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -15, Model.maxHeight = 17}},Model.Score {Model.getPoints = 7}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -12, Model.maxHeight = 22}},Model.Score {Model.getPoints = 19}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = -8, Model.maxHeight = 19}},Model.Score {Model.getPoints = -21}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 4, Model.maxHeight = 1}},Model.Score {Model.getPoints = -10}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 6, Model.maxHeight = -6}},Model.Score {Model.getPoints = 1}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 9, Model.maxHeight = -9}},Model.Score {Model.getPoints = 10}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 11, Model.maxHeight = -22}},Model.Score {Model.getPoints = -5}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 12, Model.maxHeight = 0}},Model.Score {Model.getPoints = -8}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 16, Model.maxHeight = -29}},Model.Score {Model.getPoints = 25}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 18, Model.maxHeight = 11}},Model.Score {Model.getPoints = 1}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 21, Model.maxHeight = 19}},Model.Score {Model.getPoints = -18}),(Model.League {Model.getBounds = Model.Bounds {Model.maxWidth = 24, Model.maxHeight = -4}},Model.Score {Model.getPoints = -29})]}) `shouldBe` "Zomg"
  describe "Model.Score" $ do
    it "should deserialise the score correctly" $ do
      Leaderboard.deserialiseScore "[Score - Points:21]" `shouldBe`
        (Just $ Model.Score 21)
    it "should serialise the score correctly" $ do
      Leaderboard.serialiseScore (Model.Score 22) `shouldBe`
        "[Score - Points:22]"
    it "should be able to serialise & deserialise a lot" $ do
      forAll exampleScore $ \score ->
        (Leaderboard.deserialiseScore (Leaderboard.serialiseScore score)) `shouldBe`
        Just score
  describe "serialising an entire line" $ do
    it "should be able to serialise & deserialise back & forth" $ do
      forAll arbitrary $ \attempt ->
        (Leaderboard.deserialiseAttempt (Leaderboard.serialiseAttempt attempt)) `shouldBe`
        Just attempt
    context "when there is a matching line for a given boundsStr" $ do
      it "matches the line" $ do
        Leaderboard.maybeLineContaining boundsStr nonMatchinString `shouldBe`
          Nothing
  describe "Leaderboard serialisation" $ do
    it "works" $ do
      forAll (Model.Leaderboard <$> arbitrary) $ \leaderboard ->
        (Leaderboard.deserialiseLeaderboard
           (Leaderboard.serialiseLeaderboard leaderboard)) `shouldBe`
        Just leaderboard
    context "when there is a blank file" $ do
      it "returns Nothing" $ do
        Leaderboard.maybeLineContaining boundsStr "" `shouldBe` Nothing
      -- it "this is a pointless test" $ do
      --   let exampleStrings = ["hey", show boundsStr ++ "wooo", "nope", "omg" ++ show boundsStr]
            -- genStrings = choose exampleStrings
          -- in forAll (elements exampleStrings) $ \string -> if string `Data.List.isInfixOf` `shouldBe`
          -- show boundsStr `Data.List.isPrefixOf` (Leaderboard.maybeLineContaining boundsStr string)
          -- in forAll (elements exampleStrings) $ \string -> Leaderboard.maybeLineContaining boundsStr string `shouldBe` Just "omg"--(Data.List.isInfixOf string (show boundsStr))
         -- in forAll (pure $ boundsStr) $ \bounds' ->
         --      Model.maxHeight bounds' == Model.maxHeight bounds'
  -- regex
    -- context "property Testing - no idea what I'm doing" $ do
    --   it "works" $ do
    --     -- property $ (\str -> Leaderboard.maybeLineContaining randomBounds' str)
    --     property $ \x xs -> head (x : xs) == (x :: Int)
    --       where randomBounds' = randomBounds

-- generator = generate $ choose (1, 2)
generator :: IO Integer
generator = generate $ elements [1,2,3]

-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
-- forAll <generator>         $ \<pattern> -> <property>
-- The following is a property:
  -- generators :: x -> Gen x
prop_Test = forAll (elements [1,2,3]) $ \x -> x < 5
           -- forAll is a test data generator
           -- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
           -- forAll ::  Gen a -> (a -> prop) -> Property
           -- forAll :: generator -> (prop test) -> Property

-- exampleBounds = oneof [return $ Model.Bounds 1 10]
-- classify
-- (==>) :: Testable prop => Bool -> prop -> Property
--   ==>

-- arbitrary  -- this is a common-sense generator. eg.
-- generate :: Gen a -> IO a
-- arbitrary :: Arbitrary a => Gen a

q = generate (arbitrary :: Gen Int)

-- Using generators:
-- forAll <generator> $ \<pattern> -> <property>

w = forAll (pure $ Model.Bounds 1 10) $ \bounds' -> Model.maxHeight bounds' == Model.maxHeight bounds'
  -- choose

randomBounds :: IO Model.Bounds
randomBounds = generate $ Model.Bounds <$> arbitrary <*> arbitrary

-- newtype Gen a = MkGen {
--   unGen :: QCGen -> Int -> a
-- }
-- randomBounds' :: Gen Model.Bounds

b = do
  b <- randomBounds
  print b
  print b

myList' :: Arbitrary a => Gen [a]
myList' = frequency
  [ (1, return [])
  , (4, (:) <$> arbitrary <*> myList')
  ]

bounds :: Model.Bounds
bounds = Model.Bounds 11 12

boundsStr :: String
boundsStr = "[11,12]"

yes :: [Char]
yes = "Bounds {maxWidth = 11, Model.maxHeight = 12} - Yep"

no1 :: [Char]
no1 = "Bounds {maxWidth = 12, Model.maxHeight = 12} - Nope"

no2 :: [Char]
no2 = "Bounds {maxWidth = 10, Model.maxHeight = 12} - NOOO"

matchingString :: [Char]
matchingString = no1 ++ "\n" ++ yes ++ "\n" ++ no2

nonMatchinString :: [Char]
nonMatchinString = no1 ++ "\n" ++ no2
