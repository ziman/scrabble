module Main where

import Control.Applicative
import Options.Applicative

import qualified Scrabble
import qualified Data.Yaml as Yaml
import qualified Game.WSGame.Engine as Engine

data Options = Options
  { fnLanguage :: FilePath
  , address  :: String
  , port :: Int
  , mbFnPersist :: Maybe FilePath
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
  <*> optional (strOption
    ( long "persist"
    <> short 'p'
    <> metavar "STATE.yaml"
    <> help "persist state in this file"
    ))

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

  initialState <- case mbFnPersist of
    Nothing -> Scrabble.mkInitialState fnLanguage
    Just fnPersist -> Yaml.decodeFileEither fnPersist >>= \case
      Left _ -> Scrabble.mkInitialState fnLanguage
      Right state -> pure (Scrabble.activate state)

  putStrLn $ "starting the backend at " ++ address ++ ":" ++ show port
  Engine.runGame address port initialState (Scrabble.Env mbFnPersist) Scrabble.game
