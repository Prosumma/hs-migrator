{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.Trans.Resource
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
  <*> envString (Just "") "PGPASSWORD"
  <*> envString (Just "") "PGDATABASE" 

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

-- TO DO: Break this beast up into some proper Haskell functions.
migrate :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Bool -> Maybe FilePath -> m ()
migrate shouldInit dir = do 
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ do
    logWarnS logSource $ uformat ("The directory '" % string % "' does not exist. Quitting.") path
    exitFailure
  lf <- asks (^.logFuncL)
  connectInfo <- readConnectInfo
  let dbname = connectInfo.connectDatabase
  -- Create the database if needed and if permitted.
  runResourceT $ do
    conn <- snd <$> allocate (liftIO $ connect connectInfo{connectDatabase="postgres"}) close
    runRIO (PG conn lf) $ do
      dbexists <- value1 "SELECT EXISTS (SELECT 1 FROM pg_database WHERE datname = ?)" (Only dbname)
      unless dbexists $ do
        if shouldInit
          then void $ execute_ (fromString $ "CREATE DATABASE " <> dbname) 
          else do
            logErrorS logSource $ 
              uformat ("The database " % string % " does not exist and --no-init was passed to prevent its creation.") dbname
            exitFailure
  -- Use the database
  runResourceT $ do
    conn <- snd <$> allocate (liftIO $ connect connectInfo) close
    runRIO (PG conn lf) $ do
      schemaExists <- value1 "SELECT EXISTS (SELECT 1 FROM pg_namespace WHERE nspname = ?)" (Only "__migrations" :: Only Text)
      unless schemaExists $ do
        if shouldInit 
          then void $ execute_ "CREATE SCHEMA __migrations" 
          else do
              logErrorS logSource $
                uformat ("The schema __migrations does not exist in the database " % string % " and --no-init was passed to prevent its creation.") dbname
              exitFailure
      migrationsTableExists <- value1 "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = ? AND tablename = 'migrations')" (Only "__migrations" :: Only Text)
      unless migrationsTableExists $ do
        if shouldInit
          then void $ execute_ "CREATE TABLE __migrations.migrations(id BIGSERIAL NOT NULL PRIMARY KEY)"
          else do
            logErrorS logSource $
              uformat ("The table __migrations.migrations does not exist in the database " % string % " and --no-init was passed to prevent its creation.") dbname
            exitFailure

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
