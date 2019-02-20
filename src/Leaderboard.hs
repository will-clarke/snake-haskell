module Leaderboard
  ( getLeaderboardFile
  , maybeLineContaining
  ) where

import qualified Data.List
import qualified Data.Maybe
import qualified Data.Map
import qualified Model
import qualified Data.Text
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

deserialiseLeague :: String -> Maybe League
deserialiseLeague str = Data.Text.splitOn "," . Data.Text.strip (Data.Text.pack str)

serialiseLeague :: League -> String
serialiseLeague (Bounds width height) = "[W:" ++ (show width) ++ ",H:" ++ (show height) ++ "]"

deserialiseScore :: String -> Maybe Int
deserialiseScore = undefined

serialiseScore :: Score -> String
serialiseScore = undefined

deserialiseLeaderboard :: String -> Leaderboard
deserialiseLeaderboard = undefined

serialiseLeaderboard :: Leaderboard -> String
serialiseLeaderboard = undefined

deserialiseLeaderboardScore :: String -> Maybe (Leaderboard, Score)
deserialiseLeaderboardScore = undefined

serialiseLeaderboardScore :: (Leaderboard, Score) -> String
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
