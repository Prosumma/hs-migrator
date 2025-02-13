{-# LANGUAGE OverloadedRecordDot, ScopedTypeVariables #-}

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
import RIO.List
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
  highWater <- value1_ "SELECT COALESCE(MAX(migration), '00000000000000')::TEXT FROM __migrations.migrations"
  files <- sort . filter (=~ fileRegex) <$> listDirectory path
  for_ files $ \filename -> do 
    let migration = dropExtension filename
    exists <- value1 migrationsExistsSQL (Only migration)
    unless exists $ do
      let filepath = path </> filename
      if migration < highWater 
        then logWarnS logSource $
          uformat ("The migration " % string % " has been skipped because later migrations have already been processed.") filepath 
        else do 
          logInfoS logSource $ uformat ("About to process " % string % ".") filepath 
          sql <- readFileUtf8 filepath 
          -- There's probably a better way to do this
          void $ execute_ (fromString $ Text.unpack sql)
          void $ execute insertMigrationSQL (Only migration)
          logInfoS logSource $ uformat ("Processed " % string % ".") filepath

migrate :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Bool -> Maybe FilePath -> Bool -> m ()
migrate shouldInit dir dropDatabase = do 
  let path = fromMaybe "." dir 
  exists <- doesDirectoryExist path
  unless exists $ do
    logWarnS logSource $ uformat ("The directory '" % string % "' does not exist. Quitting.") path
    exitFailure
  connectInfo <- readConnectInfo
  let dbname = connectInfo.connectDatabase
  -- Create the database if needed and if permitted.
  runConnection connectInfo{connectDatabase="postgres"} $
    createDatabaseIfNeeded dbname
  -- Use the database
  runConnection connectInfo $ do
    createSchemaIfNeeded dbname
    createMigrationsTableIfNeeded dbname
    performMigration path
  where
    createDatabaseIfNeeded dbname = do
      when dropDatabase $ void $ execute_ (fromString $ "DROP DATABASE IF EXISTS " <> dbname)
      dbexists <- value1 databaseExistsSQL (Only dbname)
      unless dbexists $ do
        if shouldInit
          then void $ execute_ (fromString $ "CREATE DATABASE " <> dbname) 
          else do
            logErrorS logSource $ uformat databaseDoesNotExistFMT dbname
            exitFailure
    createSchemaIfNeeded dbname = do
      schemaExists <- value1 schemaExistsSQL (Only "__migrations" :: Only Text)
      unless schemaExists $ do
        if shouldInit 
          then void $ execute_ "CREATE SCHEMA __migrations" 
          else do
            logErrorS logSource $ uformat migrationSchemaDoesNotExistFMT dbname
            exitFailure
    createMigrationsTableIfNeeded dbname = do
      migrationsTableExists <- value1 migrationsTableExistsSQL (Only "__migrations" :: Only Text)
      unless migrationsTableExists $ do
        if shouldInit
          then do 
            void $ execute_ "CREATE EXTENSION IF NOT EXISTS citext;"
            void $ execute_ createMigrationsTableSQL 
          else do
            logErrorS logSource $
              uformat migrationsTableDoesNotExistFMT dbname
            exitFailure

runConnection :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => ConnectInfo -> RIO (PG Connection) a -> m a
runConnection connectInfo action = do 
  lf <- asks (^.logFuncL)
  runResourceT $ do
    conn <- snd <$> allocate (connect connectInfo) close
    runRIO (PG conn lf) action

migrationsExistsSQL :: Query
migrationsExistsSQL = "SELECT EXISTS (SELECT 1 FROM __migrations.migrations WHERE migration = ?)"

insertMigrationSQL :: Query
insertMigrationSQL = "INSERT INTO __migrations.migrations(migration) VALUES(?)"

databaseExistsSQL :: Query
databaseExistsSQL = "SELECT EXISTS (SELECT 1 FROM pg_database WHERE datname = ?)"

schemaExistsSQL :: Query
schemaExistsSQL = "SELECT EXISTS (SELECT 1 FROM pg_namespace WHERE nspname = ?)"

migrationsTableExistsSQL :: Query
migrationsTableExistsSQL = "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = ? AND tablename = 'migrations')"

createMigrationsTableSQL :: Query
createMigrationsTableSQL = "CREATE TABLE __migrations.migrations(\
  \  id SERIAL NOT NULL PRIMARY KEY\
  \, migration CITEXT NOT NULL UNIQUE\
  \, migrated TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'UTC')\
  \)"

databaseDoesNotExistFMT :: Format a (String -> a)
databaseDoesNotExistFMT = "The database "
  % string
  % " does not exist and --no-init was passed to prevent its creation."
  
migrationSchemaDoesNotExistFMT :: Format a (String -> a)
migrationSchemaDoesNotExistFMT = "The schema __migrations does not exist in the database "
  % string
  % " and --no-init was passed to prevent its creation."
  
migrationsTableDoesNotExistFMT :: Format a (String -> a)
migrationsTableDoesNotExistFMT = "The table __migrations.migrations does not exist in the database "
  % string
  % " and --no-init was passed to prevent its creation."
