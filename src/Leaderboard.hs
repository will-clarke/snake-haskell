module Leaderboard
  ( getLeaderboardFile
  , maybeLineContaining
  , deserialiseLeague
  , serialiseLeague
  , deserialiseScore
  , serialiseScore
 , deserialiseAttempt
  , serialiseAttempt
  , deserialiseLeaderboard
  , serialiseLeaderboard
  ) where

import qualified Control.Monad
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Model
import qualified System.Directory
import qualified System.IO
import           System.FilePath  ((</>))
import qualified Text.Read

getLeaderboard :: IO (Maybe Model.Leaderboard)
getLeaderboard = do
  lbFile <- getLeaderboardFile
  exists <- System.Directory.doesFileExist lbFile
  if exists
     then deserialiseLeaderboard <$> System.IO.readFile lbFile
    -- Text.Read.readMaybe <$> System.IO.readFile lbFile
     -- then System.IO.readFile lbFile >>= deserialiseLeaderboard
     -- then deserialiseLeaderboard lbFile
     else return Nothing

-- setLeaderboard :: (Model.League, Model.Score) -> IO ()
-- setLeaderboard s = do
--   lb <- getLeaderboard
--   writeFile lb (show s)

-- updateLeaderboard :: Model.Leaderboard ->

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
deserialiseLeaderboard fileBody =
  let attempts :: [Maybe Model.Attempt]
      attempts = map deserialiseAttempt (lines fileBody)
      maybeAttempts :: Maybe [Model.Attempt]
      maybeAttempts = sequence attempts
   in maybeAttempts >>= \attempts' ->
        Just $ Model.Leaderboard (Data.Map.fromList $ map Model.toTuple attempts')

serialiseLeaderboard :: Model.Leaderboard -> String
serialiseLeaderboard leaderboard =
  concatMap
    (\(league, score) -> serialiseAttempt (Model.Attempt league score))
       -- serialiseLeague league ++ " -- " ++ serialiseScore score ++ "\n")
    (Data.Map.toDescList $ Model.getLeagues leaderboard)

deserialiseAttempt :: String -> Maybe Model.Attempt
deserialiseAttempt line =
  let lines :: [Data.Text.Text]
      lines = map Data.Text.strip $ Data.Text.splitOn (Data.Text.pack " -- ") (Data.Text.pack line)
      possibleLeague :: [Data.Text.Text] -> Maybe Model.League
      possibleLeague (l:xs) = deserialiseLeague (Data.Text.unpack l)
      possibleLeague _      = Nothing
      possibleScore :: [Data.Text.Text] -> Maybe Model.Score
      possibleScore (_:scoreTxt:[]) =
        deserialiseScore (Data.Text.unpack scoreTxt)
      possibleScore _ = Nothing
   in do league <- possibleLeague lines
         score <- possibleScore lines
         return (Model.Attempt league score)

serialiseAttempt :: Model.Attempt -> String
serialiseAttempt (Model.Attempt league score) = serialiseLeague league ++ " -- " ++ serialiseScore score  ++ "\n"

-- boundsString :: Model.Bounds -> String
-- boundsString (Model.Bounds width height) = "[" <> show width <> "," <> show height <> "]"





-- getLeaderboard :: Model.Bounds -> IO (Maybe Int)
-- getLeaderboard bounds = do
--   file <- getLeaderboardFile
--   exists <- System.Directory.doesFileExist file
--   if exists
--      then maybeFindHighScore bounds $ readFile file
--      else return Nothing

-- maybeFindHighScore :: Model.Bounds -> IO String -> IO (Maybe Int)
-- maybeFindHighScore bounds ioString = do
--   fileContents <- ioString
--   line <- maybeLineContaining bounds fileContents
--   dropWhile (/= '-') line

-- maybeFindHighScore
-- readMaybe <$> readFile file

-- setLeaderboard bounds :: Model.Bounds -> Int -> IO ()
-- setLeaderboard s = do
--   file <- getLeaderboardFile
--   writeFile file (show s)
