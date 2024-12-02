{-# LANGUAGE OverloadedRecordDot #-}

module Migrator.Migrate (migrate) where

import Control.Monad.Trans.Resource
import Data.Text.Read (decimal)
import Formatting
import Migrator.Internal
import Options.Applicative
import Prosumma.PG
import Prosumma.Util
import Prosumma.Util.Environment
import RIO
import RIO.Directory
import RIO.FilePath
import Text.Regex.TDFA ((=~))

import qualified RIO.Text as Text

readConnectInfo :: MonadIO m => m ConnectInfo
readConnectInfo = ConnectInfo
  <$> envString (Just "127.0.0.1") "PGHOST"
  <*> envValue  (Just 5432) decimal "PGPORT" 
  <*> envString (Just "postgres") "PGUSER"
  <*> envString (Just "") "PGPASSWORD"
  <*> envString (Just "") "PGDATABASE" 

performMigration :: FilePath -> RIO (PG Connection) ()
performMigration path = do
  files <- filter (=~ fileRegex) <$> listDirectory path
  for_ files $ \filename -> do 
    let filepath = path </> filename
    let migration = dropExtension filename
    exists <- value1 "SELECT EXISTS (SELECT 1 FROM __migrations.migrations WHERE migration = ?)" (Only migration)
    unless exists $ do
      logInfoS logSource $ uformat ("About to process " % string % ".") filepath 
      sql <- readFileUtf8 filepath 
      -- There's probably a better way to do this
      void $ execute_ (fromString $ Text.unpack sql)
      void $ execute "INSERT INTO __migrations.migrations(migration) VALUES(?)" (Only migration)
      logInfoS logSource $ uformat ("Processed " % string % ".") filepath

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
    runRIO (PG conn lf) $ createDatabaseIfNeeded dbname 
  -- Use the database
  runResourceT $ do
    conn <- snd <$> allocate (liftIO $ connect connectInfo) close
    runRIO (PG conn lf) $ do
      createSchemaIfNeeded dbname
      createMigrationsTableIfNeeded dbname
      performMigration path
  where
    createDatabaseIfNeeded dbname = do
      when dropDatabase $ void $ execute_ (fromString $ "DROP DATABASE IF EXISTS " <> dbname)
      dbexists <- value1 "SELECT EXISTS (SELECT 1 FROM pg_database WHERE datname = ?)" (Only dbname)
      unless dbexists $ do
        if shouldInit
          then void $ execute_ (fromString $ "CREATE DATABASE " <> dbname) 
          else do
            logErrorS logSource $ 
              uformat ("The database " % string % " does not exist and --no-init was passed to prevent its creation.") dbname
            exitFailure
    createSchemaIfNeeded dbname = do
      schemaExists <- value1 "SELECT EXISTS (SELECT 1 FROM pg_namespace WHERE nspname = ?)" (Only "__migrations" :: Only Text)
      unless schemaExists $ do
        if shouldInit 
          then void $ execute_ "CREATE SCHEMA __migrations" 
          else do
            logErrorS logSource $
              uformat ("The schema __migrations does not exist in the database " % string % " and --no-init was passed to prevent its creation.") dbname
            exitFailure
    createMigrationsTableIfNeeded dbname = do
      migrationsTableExists <- value1 "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = ? AND tablename = 'migrations')" (Only "__migrations" :: Only Text)
      unless migrationsTableExists $ do
        if shouldInit
          then do
            void $ execute_ "CREATE EXTENSION IF NOT EXISTS citext;"
            void $ execute_ "CREATE TABLE __migrations.migrations(id SERIAL NOT NULL PRIMARY KEY, migration CITEXT NOT NULL UNIQUE, migrated TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'UTC'))"
          else do
            logErrorS logSource $
              uformat ("The table __migrations.migrations does not exist in the database " % string % " and --no-init was passed to prevent its creation.") dbname
            exitFailure
