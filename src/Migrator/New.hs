module Migrator.New (createNewMigration) where

import Formatting
import Migrator.Internal
import Prosumma.Util
import RIO
import RIO.Directory
import RIO.FilePath
import RIO.Time
import Text.Regex.TDFA

import qualified RIO.ByteString as ByteString

createNewMigration :: (MonadReader env m, HasLogFunc env, MonadIO m) => FilePath -> Maybe FilePath -> m ()
createNewMigration desc dir = do
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ do
    logWarnS logSource $ uformat ("Directory " % string % " did not exist and was created.") path
    createDirectoryIfMissing True path 
  currentTime <- getCurrentTime
  let timePrefix = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime
  let filename = timePrefix <> "_" <> desc <> ".psql"
  unless (filename =~ fileRegex) $ do
    logErrorS logSource $ uformat ("The generated filename '" % string % "' is not valid and cannot be created.") filename
    exitFailure
  let filepath = path </> filename 
  ByteString.writeFile filepath "-- Create your new migration here."
  logInfoS logSource $ uformat ("Created " % string) filepath 