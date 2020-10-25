module Main where

import System.Random
import Data.Functor ((<&>))
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.List (stripPrefix)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Concurrent.STM

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import qualified Api

data Client = Client
  { clName :: Text
  , clConnection :: WS.Connection
  , clLetters :: [Api.Letter]
  , clScore :: Int
  , clCookie :: Api.Cookie
  }

data State = State
  { stClients :: Map Api.Cookie Client
  , stBoardSize :: (Int, Int)
  , stBoard :: Map (Int, Int) Api.Cell
  , stBag :: [Api.Letter]
  }

data Env = Env
  { envConnection :: WS.Connection
  , envState :: TVar State
  , envCookie :: Api.Cookie
  }

data Error
  = ClientError Text  -- keep the connection
  | GeneralError Text  -- kill the connection
  deriving (Eq, Ord, Show)

type Api = ReaderT Env (ExceptT Error IO)

throw :: Error -> Api a
throw = lift . throwE

throwClient :: String -> Api a
throwClient = throw . ClientError . Text.pack

throwGeneral :: String -> Api a
throwGeneral = throw . GeneralError . Text.pack

recv :: Api Api.Message_C2S
recv = do
  conn <- envConnection <$> ask
  msgBS <- liftIO $ WS.receiveData conn
  case Aeson.decode msgBS of
    Nothing -> throwGeneral $ "can't decode: " ++ show msgBS
    Just msg -> pure msg

send :: Api.Message_S2C -> Api ()
send msg = do
  conn <- envConnection <$> ask
  liftIO $ WS.sendTextData conn (Aeson.encode msg)

loop :: Api () -> Api a
loop api = do
  liftCatch catchE api $ \case
    ClientError msg -> do
      send Api.Error{ mscMessage = msg }
      loop api
    GeneralError msg -> throw $ GeneralError msg
  loop api

getClient :: Api Client
getClient = do
  cookie <- envCookie <$> ask
  st <- liftIO . readTVarIO =<< (envState <$> ask)
  case Map.lookup cookie (stClients st) of
    Nothing -> throwGeneral "client not found"
    Just client -> pure client

sendStateUpdate :: Api ()
sendStateUpdate = do
  st <- liftIO . readTVarIO =<< (envState <$> ask)
  client <- getClient

  let (rows, cols) = stBoardSize st
  send $ Api.Update $ Api.State
    { Api.stPlayers = map clName $ Map.elems $ stClients st
    , Api.stBoard = Api.Board
      { bRows = rows
      , bCols = cols
      , bCells =
        [ [ stBoard st Map.! (i, j)
          | j <- [0..cols-1]
          ]
        | i <- [0..rows-1]
        ]
      }
    , Api.stLetters = clLetters client
    }

clientLoop :: Api ()
clientLoop = do
  sendStateUpdate
  loop $ recv >>= handle

handle :: Api.Message_C2S -> Api ()
handle Api.Join{..} = do
  liftIO $ print mcsPlayerName

application :: TVar State -> WS.ServerApp
application tvState pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    cookie <- randomIO
    let env = Env
          { envConnection = connection
          , envState      = tvState
          , envCookie     = cookie
          }
    runExceptT (runReaderT clientLoop env) >>= \case
      Left err -> putStrLn $ "error: " ++ show err
      Right () -> pure ()

-- repeats are fine
symmetry :: [(Int, Int)] -> [(Int, Int)]
symmetry ijs = concat
  [ [ (i, j)
    , (15-i, j)
    , (i, 15-j)
    , (15-i, 15-j)
    , (j, i)
    , (j, 15-i)
    , (15-j, i)
    , (15-j, 15-i)
    ]
  | (i, j) <- ijs
  ]

boosts :: [(Api.Boost, [(Int, Int)])]
boosts =
  [ (Api.DoubleLetter, symmetry [(0, 3), (2, 6), (3, 7)])
  , (Api.TripleLetter, symmetry [(1, 5), (5, 5)])
  , (Api.DoubleWord, symmetry [(i, i) | i <- [1..4]])
  , (Api.TripleWord, symmetry [(0, 0), (0, 7)])
  ]

main :: IO ()
main = do
  tvState <- newTVarIO initialState
  WS.runServer "0.0.0.0" 8083
    $ application tvState
  where
    initialState = State
      { stClients = Map.empty
      , stBoardSize = (15, 15)
      , stBoard =
        Map.fromList
          [((i,j), Api.Blank Nothing) | i <- [0..14], j <- [0..14]]
        `Map.union`
          Map.fromList
            [(ij, Api.Blank $ Just boost) | (boost, ijs) <- boosts, ij <- ijs]
      , stBag = []
      }
