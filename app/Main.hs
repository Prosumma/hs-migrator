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
import RIO.Directory
import RIO.FilePath
import RIO.Time
import Text.Regex.TDFA ((=~))

import qualified RIO.ByteString as ByteString
import qualified RIO.Text as Text

logSource :: LogSource
logSource = "MIGRATE"

data Command = Migrate Bool (Maybe FilePath) Bool | New FilePath (Maybe FilePath) deriving (Eq, Show)

directoryOption :: Parser (Maybe FilePath)
directoryOption = optional $ strOption
  ( long "directory"
  <> short 'd'
  <> metavar "DIRECTORY"
  <> help "Directory for migrations (default: .)" )

initOption :: Parser Bool  
initOption = not <$> switch (long "no-init" <> help "Set this flag to disable initializing the database")

dropOption :: Parser Bool
dropOption = switch (long "drop" <> help "Drop the target database as the first step.")
  
parseCommand :: Parser Command
parseCommand = subparser (migrate <> new)
  where
    migrate = command "migrate" (info (Migrate <$> initOption <*> directoryOption <*> dropOption) (progDesc ""))
    new = command "new" (info (New <$> argument str (metavar "DESCRIPTION") <*> directoryOption) (progDesc ""))

readConnectInfo :: MonadIO m => m ConnectInfo
readConnectInfo = ConnectInfo
  <$> envString (Just "127.0.0.1") "PGHOST"
  <*> envValue (Just 5432) decimal "PGPORT" 
  <*> envString (Just "postgres") "PGUSER"
  <*> envString (Just "") "PGPASSWORD"
  <*> envString (Just "") "PGDATABASE" 

fileRegex :: Text
fileRegex = "^[0-9]{14}_[a-zA-Z0-9_-]+\\.psql$"

createNewMigration :: (MonadReader env m, HasLogFunc env, MonadIO m) => FilePath -> Maybe FilePath -> m ()
createNewMigration desc dir = do
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ createDirectoryIfMissing True path 
  currentTime <- getCurrentTime
  let timePrefix = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime
  let filename = timePrefix <> "_" <> desc <> ".psql"
  unless (filename =~ fileRegex) $ do
    logErrorS logSource $ uformat ("The generated filename '" % string % "' is not valid and cannot be created.") filename
    exitFailure
  let filepath = path </> filename 
  ByteString.writeFile filepath "-- Create your new migration here."
  logDebugS logSource $ uformat ("Created " % string) filepath 

-- TO DO: Break this beast up into some proper Haskell functions.
migrate :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Bool -> Maybe FilePath -> Bool -> m ()
migrate shouldInit dir dropDatabase = do 
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
      when dropDatabase $ void $ execute_ (fromString $ "DROP DATABASE IF EXISTS " <> dbname)
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
          then void $ execute_ "CREATE TABLE __migrations.migrations(id SERIAL NOT NULL PRIMARY KEY, migration TEXT NOT NULL UNIQUE)"
          else do
            logErrorS logSource $
              uformat ("The table __migrations.migrations does not exist in the database " % string % " and --no-init was passed to prevent its creation.") dbname
            exitFailure
      files <- filter (=~ fileRegex) <$> listDirectory path
      for_ files $ \filename -> do 
        let migration = dropExtension filename
        exists <- value1 "SELECT EXISTS (SELECT 1 FROM __migrations.migrations WHERE migration = ?)" (Only migration)
        unless exists $ do
          sql <- readFileUtf8 $ path </> filename
          -- There's probably a better way to do this
          void $ execute_ (fromString $ Text.unpack sql)
          void $ execute "INSERT INTO __migrations.migrations(migration) VALUES(?)" (Only migration)

main :: IO ()
main = do 
  options <- logOptionsHandle stderr True
  withLogFunc options $ \lf -> runRIO lf $ do
    cmd <- liftIO $ execParser opts
    case cmd of
      Migrate init dir dropDatabase -> migrate init dir dropDatabase
      New desc dir -> createNewMigration desc dir
  where
    opts = info
      (parseCommand <**> helper)
      (fullDesc <> progDesc "PostgreSQL migrations CLI tool" <> header "hs-migrator - a PostgreSQL migration manager")
