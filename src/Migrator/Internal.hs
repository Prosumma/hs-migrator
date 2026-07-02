module Migrator.Internal (fileRegex, logSource, uformat) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import Formatting
import RIO

logSource :: LogSource
logSource = "MIGRATE"

fileRegex :: Text
fileRegex = "^[0-9]{14}_[a-zA-Z0-9_-]+\\.psql$"

uformat :: Format Utf8Builder a -> a
uformat m = runFormat m (display . T.toStrict . T.toLazyText)
