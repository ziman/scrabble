module Main where

import Data.Functor ((<&>))
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Network.WebSockets as WS

data Client = Client
  { clName :: Text
  , clConnection :: WS.Connection
  }

data State = State
  { stClients :: [Client]
  }

application :: MVar State -> WS.ServerApp
application mvState pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    _clients <- readMVar mvState <&> stClients
    WS.sendTextData conn (msg :: Text)

main :: IO ()
main = do
  mvState <- newMVar initialState
  WS.runServer "0.0.0.0" 8083
    $ application mvState
  where
    initialState = State
      { stClients = []
      }
