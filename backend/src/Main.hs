module Main where

import GHC.Generics

import Data.Functor ((<&>))
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.List (stripPrefix)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as BS

import Data.Aeson.Casing
import qualified Data.Aeson as Aeson

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

data Error
  = ClientError Text  -- keep the connection
  | GeneralError Text  -- kill the connection
  deriving (Eq, Ord, Show, Generic)

jsonOptions :: Aeson.Options
jsonOptions = (aesonPrefix camelCase)
  { Aeson.sumEncoding = Aeson.defaultTaggedObject
  }

data Message_C2S
  = Join
    { mcsUserName :: Text
    }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON Message_C2S where
  parseJSON = Aeson.genericParseJSON jsonOptions

data Message_S2C
  = Update
    { mscClients :: [Text]
    }
  | Error
    { mscMessage :: Text
    }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Message_S2C where
  toJSON = Aeson.genericToJSON jsonOptions

type Api = ReaderT Env (ExceptT Error IO)

throw :: Error -> Api a
throw = lift . throwE

throwClient :: String -> Api a
throwClient = throw . ClientError . Text.pack

throwGeneral :: String -> Api a
throwGeneral = throw . GeneralError . Text.pack

recv :: Api Message_C2S
recv = do
  conn <- envConnection <$> ask
  msgBS <- liftIO $ WS.receiveData conn
  case Aeson.decode msgBS of
    Nothing -> throwGeneral $ "can't decode: " ++ show msgBS
    Just msg -> pure msg

send :: Message_S2C -> Api ()
send msg = do
  conn <- envConnection <$> ask
  liftIO $ WS.sendTextData conn (Aeson.encode msg)

loop :: Api () -> Api a
loop api = do
  liftCatch catchE api $ \case
    ClientError msg -> do
      send Error{ mscMessage = msg }
      loop api
    GeneralError msg -> throw $ GeneralError msg
  loop api

clientLoop :: Api ()
clientLoop = do
  send $ Update{ mscClients = ["hello", "world"] }
  loop $ recv >>= handle

handle :: Message_C2S -> Api ()
handle Join{..} = do
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
      Left err -> putStrLn $ "error: " ++ show err
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
