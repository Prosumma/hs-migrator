module Main (main) where

import Formatting
import Options.Applicative
import Prosumma.Util
import RIO
import RIO.ByteString
import RIO.Directory
import RIO.FilePath
import RIO.Time

data Command = Migrate | New FilePath (Maybe FilePath) deriving (Eq, Show)

directoryOption :: Parser (Maybe FilePath)
directoryOption = optional $ strOption
  ( long "directory"
  <> short 'd'
  <> metavar "DIRECTORY"
  <> help "Directory for migrations (default: .)" )

parseCommand :: Parser Command
parseCommand = subparser (migrate <> new)
  where
    migrate = command "migrate" (info (pure Migrate) (progDesc ""))
    new = command "new" (info (New <$> argument str (metavar "DESCRIPTION") <*> directoryOption) (progDesc ""))

createNewMigration :: (MonadReader env m, HasLogFunc env, MonadIO m) => FilePath -> (Maybe FilePath) -> m ()
createNewMigration desc dir = do
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ createDirectoryIfMissing True path 
  currentTime <- getCurrentTime
  let timePrefix = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime
  let filepath = path </> (timePrefix <> "_" <> desc <> ".psql")
  writeFile filepath "-- Create your new migration here."
  logDebug $ uformat ("Created " % string) filepath 

main :: IO ()
main = do 
  options <- logOptionsHandle stderr True
  withLogFunc options $ \lf -> runRIO lf $ do
    cmd <- liftIO $ execParser opts
    case cmd of
      Migrate -> logDebug "Migrate!"
      New desc dir -> createNewMigration desc dir
  where
    opts = info
      (parseCommand <**> helper)
      (fullDesc <> progDesc "PostgreSQL migrations CLI tool" <> header "pg-migrations - a PostgreSQL migration manager")
