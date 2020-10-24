module Main where

import Data.Functor ((<&>))
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent.STM
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as BS

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import qualified Network.WebSockets as WS

data Client = Client
  { clName :: Text
  , clConnection :: WS.Connection
  }

data State = State
  { stClients :: [Client]
  }

data Env = Env
  { envConnection :: WS.Connection
  , envState :: TVar State
  }

type Api = ReaderT Env (ExceptT String IO)

clientLoop :: Api ()
clientLoop = do
  pure ()
  {-
    msg <- WS.receiveData conn
    _clients <- readMVar mvState <&> stClients
    WS.sendTextData conn (msg :: Text)
  -}

application :: TVar State -> WS.ServerApp
application tvState pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    let env = Env
          { envConnection = connection
          , envState      = tvState
          }
    runExceptT (runReaderT clientLoop env) >>= \case
      Left err -> putStrLn $ "error: " ++ err
      Right () -> pure ()

main :: IO ()
main = do
  tvState <- newTVarIO initialState
  WS.runServer "0.0.0.0" 8083
    $ application tvState
  where
    initialState = State
      { stClients = []
      }
