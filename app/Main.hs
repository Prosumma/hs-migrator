module Main (main) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Data.Text.Read (decimal)
import Formatting
import Options.Applicative
import Prosumma.PG
import Prosumma.Util
import Prosumma.Util.Environment
import RIO
import RIO.ByteString
import RIO.Directory
import RIO.FilePath
import RIO.Time

logSource :: LogSource
logSource = "MIGRATE"

data Command = Migrate Bool (Maybe FilePath) | New FilePath (Maybe FilePath) deriving (Eq, Show)

directoryOption :: Parser (Maybe FilePath)
directoryOption = optional $ strOption
  ( long "directory"
  <> short 'd'
  <> metavar "DIRECTORY"
  <> help "Directory for migrations (default: .)" )

initOption :: Parser Bool  
initOption = not <$> switch (long "no-init" <> help "Set this flag to disable initializing the database")
  
parseCommand :: Parser Command
parseCommand = subparser (migrate <> new)
  where
    migrate = command "migrate" (info (Migrate <$> initOption <*> directoryOption) (progDesc ""))
    new = command "new" (info (New <$> argument str (metavar "DESCRIPTION") <*> directoryOption) (progDesc ""))

readConnectInfo :: MonadIO m => m ConnectInfo
readConnectInfo = ConnectInfo
  <$> envString (Just "127.0.0.1") "PGHOST"
  <*> envValue (Just 5432) decimal "PGPORT" 
  <*> envString (Just "postgres") "PGUSER"
  <*> envString (Just "") "PGDATABASE" 
  <*> envString (Just "") "PGPASSWORD"

createNewMigration :: (MonadReader env m, HasLogFunc env, MonadIO m) => FilePath -> Maybe FilePath -> m ()
createNewMigration desc dir = do
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ createDirectoryIfMissing True path 
  currentTime <- getCurrentTime
  let timePrefix = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime
  let filepath = path </> (timePrefix <> "_" <> desc <> ".psql")
  writeFile filepath "-- Create your new migration here."
  logDebugS logSource $ uformat ("Created " % string) filepath 

migrate :: (MonadReader env m, HasLogFunc env, MonadIO m) => Bool -> Maybe FilePath -> m ()
migrate _ dir = do 
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ do
    logWarnS logSource $ uformat ("The directory '" % string % "' does not exist. Quitting.") path
    exitFailure
  conn <- liftIO $ readConnectInfo >>= connect 
  lf <- asks (^.logFuncL)
  runRIO (PG conn lf) $ void $ execute_ "PERFORM 1" 

main :: IO ()
main = do 
  options <- logOptionsHandle stderr True
  withLogFunc options $ \lf -> runRIO lf $ do
    cmd <- liftIO $ execParser opts
    case cmd of
      Migrate init dir -> migrate init dir 
      New desc dir -> createNewMigration desc dir
  where
    opts = info
      (parseCommand <**> helper)
      (fullDesc <> progDesc "PostgreSQL migrations CLI tool" <> header "hs-migrator - a PostgreSQL migration manager")
