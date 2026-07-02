{-# LANGUAGE GADTs, NamedFieldPuns, RecordWildCards #-}

module Migrator.PG
  ( close,
    connect,
    connectPostgreSQL,
    execute_,
    execute,
    parseConnectInfo,
    query_,
    query,
    query1_,
    query1,
    runPG,
    value1_,
    value1,
    withTransaction_,
    withTransaction,
    Connection,
    ConnectInfo (..),
    Only (..),
    PG (..),
    Query,
    QueryRunner,
    TransactionRunner,
    RPG,
  )
where

import Control.Composition
import Data.Attoparsec.Text
import Data.Char
import Data.Functor
import Data.List.Safe
import Data.String.Conversions (cs)
import Data.String.Conversions.Monomorphic
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connect, connectPostgreSQL, defaultConnectInfo, formatQuery)
import qualified Database.PostgreSQL.Simple as Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import RIO hiding (log)
import qualified RIO.Map as Map

sqlLogSource :: LogSource
sqlLogSource = "SQL"

-- | A SQL statement, optionally paired with parameters to bind into it.
data SQLQuery where
  SQLQuery :: Query -> SQLQuery
  ParameterizedSQLQuery :: (ToRow q) => Query -> q -> SQLQuery

formatSQLQuery :: Connection -> SQLQuery -> IO Text
formatSQLQuery _ (SQLQuery sql) = return $ cs $ fromQuery sql
formatSQLQuery conn (ParameterizedSQLQuery sql q) = cs <$> formatQuery conn sql q

log :: (MonadReader env m, HasLogFunc env, MonadIO m) => Connection -> SQLQuery -> m ()
log conn sql = liftIO (formatSQLQuery conn sql) >>= logDebugS sqlLogSource . display

-- | A type which can invoke SQL queries. Migrator only ever runs against a
-- single @Connection@ (never a pool), so this exists solely to give
-- @execute@\/@query@ a uniform interface over 'Connection' and 'PG'.
class QueryRunner c where
  runExecute :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => SQLQuery -> c -> m Int64
  runQuery :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m, FromRow r) => SQLQuery -> c -> m [r]

instance QueryRunner Connection where
  runExecute q@(SQLQuery sql) conn =
    log conn q >> liftIO (Simple.execute_ conn sql)
  runExecute q@(ParameterizedSQLQuery sql params) conn =
    log conn q >> liftIO (Simple.execute conn sql params)
  runQuery q@(SQLQuery sql) conn =
    log conn q >> liftIO (Simple.query_ conn sql)
  runQuery q@(ParameterizedSQLQuery sql params) conn =
    log conn q >> liftIO (Simple.query conn sql params)

-- | A type which can execute an action within a transaction.
class TransactionRunner c where
  transact :: (MonadUnliftIO m) => c -> m a -> m a

instance TransactionRunner Connection where
  transact conn action = withRunInIO $ \runInIO -> Simple.withTransaction conn (runInIO action)

-- | Contains the minimum fields needed to call one of the query functions
-- exported from this module, such as @execute@, @query@, @value1@, etc.
--
-- Of course, @r@ must implement @QueryRunner@ for it to work.
data PG r = PG {pgQueryRunner :: r, pgLogFunc :: LogFunc}

type RPG r = RIO (PG r)

runPG :: (MonadIO m) => r -> LogFunc -> RPG r a -> m a
runPG runner logFunc = runRIO (PG runner logFunc)

instance HasLogFunc (PG r) where
  logFuncL = lens pgLogFunc $ \context pgLogFunc -> context {pgLogFunc}

instance (QueryRunner r) => QueryRunner (PG r) where
  runExecute sql PG {..} = runExecute sql pgQueryRunner
  runQuery sql PG {..} = runQuery sql pgQueryRunner

execute_ ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m) =>
  Query ->
  m Int64
execute_ sql = ask >>= runExecute (SQLQuery sql)

execute ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, ToRow q) =>
  Query ->
  q ->
  m Int64
execute sql q = ask >>= runExecute (ParameterizedSQLQuery sql q)

query_ ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, FromRow r) =>
  Query ->
  m [r]
query_ sql = ask >>= runQuery (SQLQuery sql)

query ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, ToRow q, FromRow r) =>
  Query ->
  q ->
  m [r]
query sql q = ask >>= runQuery (ParameterizedSQLQuery sql q)

query1_ ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, MonadThrow m, FromRow r) =>
  Query ->
  m r
query1_ = query_ >=> head

query1 ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, ToRow q, MonadThrow m, FromRow r) =>
  Query ->
  q ->
  m r
query1 = query >=*> head

value1_ ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, MonadThrow m, FromField v) =>
  Query ->
  m v
value1_ = (fromOnly <$>) . query1_

value1 ::
  (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, MonadThrow m, ToRow q, FromField v) =>
  Query ->
  q ->
  m v
value1 = (fromOnly <$>) .* query1

withTransaction :: (MonadReader env m, TransactionRunner env, MonadUnliftIO m) => m a -> m a
withTransaction action = ask >>= flip transact action

withTransaction_ :: (MonadReader env m, TransactionRunner env, MonadUnliftIO m) => m a -> m ()
withTransaction_ = void . withTransaction

type ConnectionStringMap = Map Text Text

parseConnectionString :: Text -> Either String ConnectionStringMap
parseConnectionString = parseOnly connectionStringParser

connectionStringParser :: Parser ConnectionStringMap
connectionStringParser = Map.fromList <$> pair `sepBy` space
  where
    pair = (,) <$> key <*> (char '=' *> value)
    key = takeTill (== '=')
    value = quotedValue <|> plainValue
    plainValue = takeTill isSpace
    quotedValue = char '\'' *> escapedString '\'' <* char '\''
    escapedString quoteChar = scan False $ \escaping c ->
      case (escaping, c) of
        (True, _) -> Just False
        (False, ch) -> if ch == quoteChar then Nothing else Just (ch == '\\')

parseConnectInfo :: Text -> Either String ConnectInfo
parseConnectInfo connectionString = do
  keyValues <- parseConnectionString connectionString
  let lookup = lookupIn keyValues
  let host = lookup "localhost" "host"
  let dbname = lookup "postgres" "dbname"
  let user = lookup "postgres" "user"
  let password = lookup "" "password"
  return
    defaultConnectInfo
      { connectHost = host,
        connectUser = user,
        connectDatabase = dbname,
        connectPassword = password
      }
  where
    lookupIn :: Map Text Text -> Text -> Text -> String
    lookupIn keyValues def key = toString $ fromMaybe def $ Map.lookup key keyValues
