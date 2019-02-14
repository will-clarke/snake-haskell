module Leaderboard
  ( getLeaderboardFile
  , maybeLineContaining
  ) where

import qualified Data.List
import qualified Data.Maybe
import qualified Model
import qualified System.Directory
import           System.FilePath  ((</>))

getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- System.Directory.getXdgDirectory System.Directory.XdgData "snake"
  System.Directory.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")

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

maybeLineContaining :: Model.Bounds -> String -> Maybe String
maybeLineContaining bounds linesString =
  Data.Maybe.listToMaybe $ filter (Data.List.isPrefixOf (show bounds)) $ lines linesString
-- maybeFindHighScore
-- readMaybe <$> readFile file

-- setHighScore bounds :: Model.Bounds -> Int -> IO ()
-- setHighScore s = do
--   file <- getLeaderboardFile
--   writeFile file (show s)
