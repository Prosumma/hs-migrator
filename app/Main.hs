module Main (main) where

import Migrator
import Options.Applicative
import RIO

data Command = Migrate Bool (Maybe FilePath) Bool | New FilePath (Maybe FilePath) deriving (Eq, Show)

directoryOption :: Parser (Maybe FilePath)
directoryOption = optional $ strOption $
     long "directory"
  <> short 'd'
  <> metavar "DIRECTORY"
  <> help "Directory for migrations (default: .)"

initOption :: Parser Bool  
initOption = not <$> noInit 
  where
    noInit = switch $
         long "no-init"
      <> help "Set this flag to disable initializing the database"

dropOption :: Parser Bool
dropOption = switch $
     long "drop"
  <> help "Drop the target database as the first step."
  
parseCommand :: Parser Command
parseCommand = subparser $ migrate <> new 
  where
    migrate = command "up" (info (Migrate <$> initOption <*> directoryOption <*> dropOption) (progDesc "Perform a migration"))
    new = command "new" (info (New <$> argument str (metavar "DESCRIPTION") <*> directoryOption) (progDesc "Create a new migration step"))

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
