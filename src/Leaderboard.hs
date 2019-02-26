module Leaderboard
  ( getLeaderboardFile
  , maybeLineContaining
  , deserialiseLeague
  , serialiseLeague
  , deserialiseScore
  ) where

import qualified Control.Monad
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Model
import qualified System.Directory
import           System.FilePath  ((</>))
import qualified Text.Read

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
