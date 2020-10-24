module Main where

import GHC.Generics

import Data.Functor ((<&>))
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as BS

import Data.Aeson

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Concurrent.STM

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

data Message_C2S
  = Join
    { mcsUserName :: Text
    }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Message_C2S

data Message_S2C
  = Update
    { mscClients :: [Text]
    }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Message_S2C

type Api = ReaderT Env (ExceptT String IO)

throw :: String -> Api a
throw = lift . throwE

recv :: Api Message_C2S
recv = do
  conn <- envConnection <$> ask
  msgBS <- liftIO $ WS.receiveData conn
  case decode msgBS of
    Nothing -> throw $ "can't decode: " ++ show msgBS
    Just msg -> pure msg

send :: Message_S2C -> Api ()
send msg = do
  conn <- envConnection <$> ask
  liftIO $ WS.sendTextData conn (encode msg)

clientLoop :: Api ()
clientLoop = do
  send $ Update{ mscClients = ["hello", "world"] }
  forever $
    recv >>= \case
      Join{..} -> do
        liftIO $ print mcsUserName
        send $ Update{ mscClients = ["woo", "boo"] }

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
