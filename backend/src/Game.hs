module Game where

import Prelude hiding (log)

import Data.Function
import Data.Foldable
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM

import qualified Network.WebSockets as WS

import qualified Api

data Player = Player
  { clName :: Text
  , clConnection :: Maybe WS.Connection
  , clLetters :: [Api.Letter]
  , clScore :: Int
  , clCookie :: Api.Cookie
  }

instance Show Player where
  show c = show (clName c, clCookie c, clScore c, clLetters c)

data State = State
  { stPlayers :: Map Api.Cookie Player
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
  = SoftError Text  -- keep the connection
  | HardError Text  -- kill the connection
  deriving (Eq, Ord)

instance Show Error where
  show (SoftError msg) = "soft error: " ++ Text.unpack msg
  show (HardError msg) = "hard error: " ++ Text.unpack msg

data Effect
  = Send WS.Connection Api.Message_S2C
  | Close WS.Connection
  | Log String

type Game =
  RWST
    Env
    [Effect]
    ()
    (ExceptT Error STM)

throw :: Error -> Game a
throw = lift . throwE

throwSoft :: String -> Game a
throwSoft = throw . SoftError . Text.pack

throwHard :: String -> Game a
throwHard = throw . HardError . Text.pack

liftSTM :: STM a -> Game a
liftSTM = lift . lift

send :: WS.Connection -> Api.Message_S2C -> Game ()
send conn msg = tell [Send conn msg]

close :: WS.Connection -> Game ()
close conn = tell [Close conn]

log :: String -> Game ()
log msg = tell [Log msg]

readTVar :: TVar a -> Game a
readTVar = liftSTM . STM.readTVar

writeTVar :: TVar a -> a -> Game ()
writeTVar tv = liftSTM . STM.writeTVar tv

modifyTVar :: TVar a -> (a -> a) -> Game ()
modifyTVar tv = liftSTM . STM.modifyTVar tv

getCookie :: Game Api.Cookie
getCookie = envCookie <$> ask

getConnection :: Game WS.Connection
getConnection = envConnection <$> ask

getState :: Game State
getState = readTVar . envState =<< ask

setState :: State -> Game ()
setState st = do
  tvState <- envState <$> ask
  writeTVar tvState st

modifyState :: (State -> State) -> Game ()
modifyState f = do
  tvState <- envState <$> ask
  modifyTVar tvState f

getPlayer :: Game Player
getPlayer = do
  st <- getState
  cookie <- envCookie <$> ask
  case Map.lookup cookie (stPlayers st) of
    Nothing -> throwHard $ "could not resolve cookie " ++ show cookie
    Just player -> pure player

sendStateUpdate :: WS.Connection -> Player -> State -> Game ()
sendStateUpdate conn player st = do
  let (rows, cols) = stBoardSize st
  send conn $ Api.Update $ Api.State
    { Api.stPlayers = map clName $ Map.elems $ stPlayers st
    , Api.stBoard = Api.Board  -- we could precompute this
      { bRows = rows
      , bCols = cols
      , bCells =
        [ [ stBoard st Map.! (i, j)
          | j <- [0..cols-1]
          ]
        | i <- [0..rows-1]
        ]
      }
    , Api.stLetters = clLetters player
    , Api.stName    = clName player
    , Api.stCookie  = clCookie player
    }

broadcastStateUpdate :: Game ()
broadcastStateUpdate = do
  st <- getState
  for_ (Map.elems $ stPlayers st) $ \player ->
    case clConnection player of
      Nothing -> pure ()  -- can't update
      Just conn -> sendStateUpdate conn player st

handle :: Api.Message_C2S -> Game ()
handle Api.Join{mcsPlayerName} = do
  st <- getState
  cookie <- getCookie

  -- first check if the username happens to be an old cookie
  let oldCookie = Api.Cookie mcsPlayerName
  case Map.lookup oldCookie (stPlayers st) of
    -- it is the old cookie, take over session
    Just oldPlayer -> do
      log $ show cookie ++ " resurrects player " ++ show (clCookie oldPlayer, clName oldPlayer)

      -- replace the player
      setState st
        { stPlayers = stPlayers st
          & Map.insert cookie oldPlayer{ clCookie = cookie }
          & Map.delete oldCookie
        }

      -- close the connection in case it's still alive
      case clConnection oldPlayer of
        Just conn -> close conn
        Nothing -> log $ "  -> old player already dead"

    Nothing -> do
      -- create a new player
      let (letters, rest) = splitAt 8 (stBag st)
      connection <- getConnection
      setState st
        { stPlayers = stPlayers st
          & Map.insert cookie Player
            { clName = mcsPlayerName
            , clConnection = Just connection
            , clLetters = letters
            , clScore = 0
            , clCookie = cookie
            }
          , stBag = rest
          }
      log $ show cookie ++ " is a new player"

  broadcastStateUpdate

handle _ = throwSoft "not implemented yet"

{-
handle Api.Drop{mcsI, mcsJ, mcsLetter} = error "undefined"
  cookie <- envCookie <$> ask
  tvState <- envState <$> ask

  liftIO $ do
    result <- atomically $ do
      st <- readTVar tvState
      -- TODO: atomic monad wrapper above api
      -- tvar read/write
      -- writer [Broadcast Message | Send Message]
      -- errorT [General | User]
      -- state [game state]
      -- reader [env]
-}
