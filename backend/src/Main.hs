module Main where

import System.Random
import Data.Functor
import Data.Function
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as Exception

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import qualified Api

data Client = Client
  { clName :: Text
  , clConnection :: Maybe WS.Connection
  , clLetters :: [Api.Letter]
  , clScore :: Int
  , clCookie :: Api.Cookie
  }

instance Show Client where
  show c = show (clName c, clCookie c, clScore c, clLetters c)

data State = State
  { stClients :: Map Api.Cookie Client
  , stBoardSize :: (Int, Int)
  , stBoard :: Map (Int, Int) Api.Cell
  , stBag :: [Api.Letter]
  }
  deriving Show

data Env = Env
  { envConnection :: WS.Connection
  , envState :: TVar State
  , envCookie :: Api.Cookie
  }

data Error
  = ClientError Text  -- keep the connection
  | GeneralError Text  -- kill the connection
  deriving (Eq, Ord)

instance Show Error where
  show (ClientError msg) = "client error: " ++ Text.unpack msg
  show (GeneralError msg) = "general error: " ++ Text.unpack msg

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
  msgBS <- liftIO $
    (Right <$> WS.receiveData conn)
      `Exception.catch` \e -> pure $ Left (show (e :: WS.ConnectionException))
  case Aeson.decode <$> msgBS of
    Left err -> throwGeneral $ "can't recv: " ++ err
    Right Nothing -> throwGeneral $ "can't decode: " ++ show msgBS
    Right (Just msg) -> pure msg

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
    , Api.stName    = clName client
    , Api.stCookie  = clCookie client
    }

clientLoop :: Api ()
clientLoop = loop (recv >>= handle)

-- send close and receive all remaining messages
closeConnection :: Api.Cookie -> WS.Connection -> IO ()
closeConnection cookie conn =
  void $ forkIO $ do
    Exception.handle @SomeException (\_ -> pure ()) $ do
      WS.sendClose conn (Aeson.encode $ Api.Error "connection replaced")
      forever (void $ WS.receive conn)
    putStrLn $ show cookie ++ ": connection closed"

handle :: Api.Message_C2S -> Api ()
handle Api.Join{mcsPlayerName} = do
  tvState <- envState <$> ask
  cookie <- envCookie <$> ask
  connection <- envConnection <$> ask

  liftIO $ do
    result <- atomically $ do
      st <- readTVar tvState

      -- first check if the username happens to be an old cookie
      let oldCookie = Api.Cookie mcsPlayerName
      case Map.lookup oldCookie (stClients st) of
        -- it is the old cookie, take over session
        Just oldClient -> do
          writeTVar tvState $
            st{ stClients = stClients st
              & Map.insert cookie oldClient{ clCookie = cookie }
              & Map.delete oldCookie
              }
          pure $ Left oldClient

        Nothing -> do
          writeTVar tvState $
            st{ stClients = stClients st
              & Map.insert cookie Client
                { clName = mcsPlayerName
                , clConnection = Just connection
                , clLetters = []
                , clScore = 0
                , clCookie = cookie
                }
              }
          pure $ Right ()

    case result of
      Left client -> do
        putStrLn $ show cookie ++ " resurrects client " ++ show (clCookie client, clName client)
        case clConnection client of
          Just conn -> closeConnection (clCookie client) conn
          Nothing -> putStrLn $ "  -> old client already dead"

      Right () -> putStrLn $ show cookie ++ " is a new client"

  sendStateUpdate

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

    result <-
      runExceptT (runReaderT clientLoop env)
        `Exception.catch`
          \e -> pure $ Left $ GeneralError $ Text.pack $ show (e :: SomeException)

    case result of
      Left err -> putStrLn $ "client " ++ show cookie ++ ": " ++ show err
      Right () -> putStrLn $ "client " ++ show cookie ++ " quit quietly"

    -- mark client as dead
    atomically $ do
      modifyTVar tvState $ \st ->
        st{ stClients =
            Map.adjust
              (\c -> c{ clConnection = Nothing })
              cookie
              (stClients st)
          }

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
          [(ij, Api.Blank $ Just boost) | (boost, ijs) <- boosts, ij <- ijs]
        `Map.union`
          Map.fromList
            [((i,j), Api.Blank Nothing) | i <- [0..14], j <- [0..14]]
      , stBag = []
      }
