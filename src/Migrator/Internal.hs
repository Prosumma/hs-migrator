module Migrator.Internal (fileRegex, logSource) where

import RIO

logSource :: LogSource
logSource = "MIGRATE"

fileRegex :: Text
fileRegex = "^[0-9]{14}_[a-zA-Z0-9_-]+\\.psql$"
