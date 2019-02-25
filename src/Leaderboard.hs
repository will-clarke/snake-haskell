module Leaderboard
  ( getLeaderboardFile
  , maybeLineContaining
  , deserialiseLeague
  , serialiseLeague
  , deserialiseScore
  ) where

import qualified Data.List
import qualified Data.Maybe
import qualified Data.Map
import qualified Model
import qualified Data.Text
import qualified Text.Read
import qualified System.Directory
import           System.FilePath  ((</>))

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
  let textStr :: Data.Text.Text
      textStr = Data.Text.pack str
      strippedTxt :: Data.Text.Text
      strippedTxt = Data.Text.dropAround (\c -> c == '[' || c == ']') . Data.Text.strip $
        textStr
      splitTxts :: [Data.Text.Text]
      splitTxts =  map Data.Text.strip $ Data.Text.splitOn (Data.Text.pack ",") strippedTxt
      possibleTextContaining :: String -> Maybe Data.Text.Text
      possibleTextContaining s =
        Data.Maybe.listToMaybe $
        Data.List.filter (Data.Text.isInfixOf $ Data.Text.pack s) splitTxts
      possibleIntContaining :: String -> Maybe Int
      possibleIntContaining s =
        let txtToRemove = Data.Text.pack $ s ++ ":"
         in (Data.Text.unpack <$>
             (possibleTextContaining s >>= Data.Text.stripPrefix txtToRemove)) >>=
            Text.Read.readMaybe
      possibleWidth = possibleIntContaining "W"
      possibleHeight = possibleIntContaining "H"
   in (Model.Bounds <$> possibleWidth <*> possibleHeight) >>= \b ->
        Just (Model.League b)

serialiseLeague :: Model.League -> String
serialiseLeague (Model.League (Model.Bounds width height)) = "[W:" ++ (show width) ++ ",H:" ++ (show height) ++ "]"

deserialiseScore :: String -> Maybe Model.Score
deserialiseScore = undefined

serialiseScore :: Model.Score -> String
serialiseScore = undefined

deserialiseLeaderboard :: String -> Model.Leaderboard
deserialiseLeaderboard = undefined

serialiseLeaderboard :: Model.Leaderboard -> String
serialiseLeaderboard = undefined

deserialiseLeaderboardScore :: String -> Maybe (Model.Leaderboard, Model.Score)
deserialiseLeaderboardScore = undefined

serialiseLeaderboardScore :: (Model.Leaderboard, Model.Score) -> String
serialiseLeaderboardScore = undefined

-- boundsString :: Model.Bounds -> String
-- boundsString (Model.Bounds width height) = "[" <> show width <> "," <> show height <> "]"





-- getHighScore :: Model.Bounds -> IO (Maybe Int)
-- getHighScore bounds = do
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

-- setHighScore bounds :: Model.Bounds -> Int -> IO ()
-- setHighScore s = do
--   file <- getLeaderboardFile
--   writeFile file (show s)
