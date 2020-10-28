module Main where

import Options.Applicative

import qualified Engine
import qualified Scrabble

data Options = Options
  { fnLanguage :: String
  , address  :: String
  , port :: Int
  }

options :: Parser Options
options = Options
  <$> strArgument
    ( metavar "LANG.yaml"
    <> help "language description file"
    )
  <*> strOption
    ( long "address"
    <> short 'a'
    <> metavar "ADDRESS"
    <> value "0.0.0.0"
    <> help "address to listen on"
    )
  <*> option auto
    ( long "port"
    <> short 'p'
    <> metavar "PORT"
    <> value 8083
    <> help "port to listen on"
    )

parseOptions :: IO Options
parseOptions = execParser $
  info (options <**> helper)
    ( fullDesc
    <> header "Scrabble backend"
    <> progDesc "Run a backend for Scrabble."
    )

main :: IO ()
main = do
  Options{..} <- parseOptions
  initialState <- Scrabble.mkInitialState fnLanguage

  putStrLn $ "starting the backend at " ++ address ++ ":" ++ show port
  Engine.runGame address port initialState Scrabble.game
