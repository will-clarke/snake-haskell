module Leaderboard
  ( getLeaderboardFile
  , getLeaderboard
  , maybeLineContaining
  , deserialiseLeague
  , serialiseLeague
  , deserialiseScore
  , serialiseScore
  , deserialiseAttempt
  , serialiseAttempt
  , deserialiseLeaderboard
  , serialiseLeaderboard
  , isHighScore
  , writeLeaderboard
  ) where

import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Model
import qualified System.Directory
import           System.FilePath  ((</>))
import qualified System.IO
import qualified Text.Read

-- This is the main Leaderboard function.. that reads, updates & writes the leaderboard file
writeLeaderboard :: Model.Attempt -> IO Model.Leaderboard
writeLeaderboard attempt@(Model.Attempt _league _score) = do
  lbFile <- getLeaderboardFile
  lbFileBody <- System.IO.readFile lbFile
  exists <- System.Directory.doesFileExist lbFile
  if exists
    then updateAndWriteLeaderboard
           (Leaderboard.deserialiseLeaderboard lbFileBody)
           attempt
           lbFile
    else writeSingleAttemptToLeaderboard attempt lbFile

writeSingleAttemptToLeaderboard :: Model.Attempt -> FilePath -> IO Model.Leaderboard
writeSingleAttemptToLeaderboard attempt filepath = do
  _ <- writeFile filepath serialisedLB
  return leaderboard
  where
    leaderboard = Leaderboard.pure attempt
    serialisedLB = Leaderboard.serialiseLeaderboard leaderboard

updateAndWriteLeaderboard ::
     Maybe Model.Leaderboard
  -> Model.Attempt
  -> FilePath
  -> IO Model.Leaderboard
updateAndWriteLeaderboard (Just oldLeaderboard) attempt filepath = do
  _ <- writeFile filepath serialisedNewLB
  return newLeaderboard
  where
    newLeaderboard = updateLeaderboard oldLeaderboard attempt
    serialisedNewLB = Leaderboard.serialiseLeaderboard newLeaderboard

updateAndWriteLeaderboard Nothing attempt filepath = writeSingleAttemptToLeaderboard attempt filepath

updateLeaderboard :: Model.Leaderboard -> Model.Attempt -> Model.Leaderboard
updateLeaderboard initialLB (Model.Attempt league score) =
  Model.Leaderboard
    (Data.Map.insertWithKey
       (\_key newVal oldVal ->
          if newVal >= oldVal
            then newVal
            else oldVal)
       league
       score
       (Model.getLeaguesAndScores initialLB))


isHighScore :: Model.Attempt -> Model.Leaderboard -> Bool
isHighScore attempt leaderboard =
  serialiseAttempt attempt `Data.List.isInfixOf` serialiseLeaderboard leaderboard

pure :: Model.Attempt -> Model.Leaderboard
pure attempt = Model.Leaderboard (Data.Map.fromList [ Model.toTuple attempt ] )

getLeaderboard :: IO (Maybe Model.Leaderboard)
getLeaderboard = do
  lbFile <- getLeaderboardFile
  exists <- System.Directory.doesFileExist lbFile
  if exists
    then deserialiseLeaderboard <$> System.IO.readFile lbFile
     else return Nothing

getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- System.Directory.getXdgDirectory System.Directory.XdgData "snake"
  System.Directory.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")

maybeLineContaining :: String -> String -> Maybe String
maybeLineContaining s linesString =
  Data.Maybe.listToMaybe $ filter (Data.List.isPrefixOf s) $ lines linesString

-- TODO: This is a horrible way of parsing. Is there a better way? ¯\_(ツ)_/¯
deserialiseLeague :: String -> Maybe Model.League
deserialiseLeague str =
  let txt :: Maybe Data.Text.Text
      txt =
        Data.Text.strip <$>
        Data.Text.stripPrefix
          (Data.Text.pack "League -")
          (Data.Text.strip .
           Data.Text.dropAround (\c -> c == '[' || c == ']') .
           Data.Text.strip . Data.Text.pack $
           str)
      attrs :: Maybe [Data.Text.Text]
      attrs = Data.Text.split (== ',') <$> txt
      attrMatching :: Data.Text.Text -> Maybe Data.Text.Text
      attrMatching t =
        ((filter (Data.Text.isPrefixOf t) <$> attrs) >>= Data.Maybe.listToMaybe) >>=
        Data.Text.stripPrefix t
      attr :: String -> Maybe Int
      attr s =
        (Text.Read.readMaybe . Data.Text.unpack) =<<
        attrMatching (Data.Text.pack s)
      possibleWidth = attr "Width:"
      possibleHeight = attr "Height:"
   in (Model.Bounds <$> possibleWidth <*> possibleHeight) >>= \b ->
        Just (Model.League b)

serialiseLeague :: Model.League -> String
serialiseLeague (Model.League (Model.Bounds width height)) =
  "[League - Width:" ++ show width ++ ",Height:" ++ show height ++ "]"

deserialiseScore :: String -> Maybe Model.Score
deserialiseScore s =
  let str :: Maybe String
      str =
        Data.Text.unpack <$>
        (Data.Text.init <$>
         Data.Text.stripPrefix (Data.Text.pack "[Score - Points:")
          (Data.Text.pack s))
      points :: Maybe Int
      points = Text.Read.readMaybe =<< str
   in Model.Score <$> points

serialiseScore :: Model.Score -> String
serialiseScore score = "[Score - Points:" ++ show (Model.getPoints score) ++ "]"

deserialiseLeaderboard :: String -> Maybe Model.Leaderboard
deserialiseLeaderboard lbFileBody =
  let attempts :: [Maybe Model.Attempt]
      attempts = map deserialiseAttempt (lines lbFileBody)
      maybeAttempts :: Maybe [Model.Attempt]
      maybeAttempts = sequence attempts
   in maybeAttempts >>= \attempts' ->
        Just $ Model.Leaderboard (Data.Map.fromList $ map Model.toTuple attempts')

serialiseLeaderboard :: Model.Leaderboard -> String
serialiseLeaderboard leaderboard =
  concatMap
    (\(league, score) -> serialiseAttempt (Model.Attempt league score))
    (Data.Map.toDescList $ Model.getLeaguesAndScores leaderboard)

deserialiseAttempt :: String -> Maybe Model.Attempt
deserialiseAttempt line =
  let lines' :: [Data.Text.Text]
      lines' = map Data.Text.strip $ Data.Text.splitOn (Data.Text.pack " -- ") (Data.Text.pack line)
      possibleLeague :: [Data.Text.Text] -> Maybe Model.League
      possibleLeague (l:_xs) = deserialiseLeague (Data.Text.unpack l)
      possibleLeague _      = Nothing
      possibleScore :: [Data.Text.Text] -> Maybe Model.Score
      possibleScore [_,scoreTxt] =
        deserialiseScore (Data.Text.unpack scoreTxt)
      possibleScore _ = Nothing
   in do league <- possibleLeague lines'
         score <- possibleScore lines'
         return (Model.Attempt league score)

serialiseAttempt :: Model.Attempt -> String
serialiseAttempt (Model.Attempt league score) = serialiseLeague league ++ " -- " ++ serialiseScore score  ++ "\n"
