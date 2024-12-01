module Main (main) where

import Formatting
import Options.Applicative
import Prosumma.Util
import RIO

data Command = Migrate | New FilePath deriving (Eq, Show)

parseCommand :: Parser Command
parseCommand = subparser (migrate <> new)
  where
    migrate = command "migrate" (info (pure Migrate) (progDesc ""))
    new = command "new" (info (New <$> argument str (metavar "DESCRIPTION")) (progDesc ""))

main :: IO ()
main = do 
  options <- logOptionsHandle stderr True
  withLogFunc options $ \lf -> runRIO lf $ do
    cmd <- liftIO $ execParser opts
    case cmd of
      Migrate -> logDebug "Migrate!"
      New fp -> logDebug $ uformat ("New: " % string) fp 
  where
    opts = info (parseCommand <**> helper) ( fullDesc <> progDesc "PostgreSQL migrations CLI tool" <> header "pg-migrations - a PostgreSQL migration manager" )