module Game where

import Prelude hiding (log)

import Data.Function
import Data.Foldable
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM

import qualified Network.WebSockets as WS

import qualified Api

data Player = Player
  { pName :: Text
  , pConnection :: Maybe WS.Connection
  , pLetters :: [Api.Letter]
  , pScore :: Int
  , pCookie :: Api.Cookie
  }

instance Show Player where
  show c = show (pName c, pCookie c, pScore c, pLetters c)

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
  = SoftError String  -- keep the connection
  | HardError String  -- kill the connection
  deriving (Eq, Ord)

instance Show Error where
  show (SoftError msg) = "soft error: " ++ msg
  show (HardError msg) = "hard error: " ++ msg

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
throwSoft = throw . SoftError

throwHard :: String -> Game a
throwHard = throw . HardError

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
    { Api.stPlayers = map pName $ Map.elems $ stPlayers st
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
    , Api.stLetters = pLetters player
    , Api.stName    = pName player
    , Api.stCookie  = pCookie player
    }

broadcastStateUpdate :: Game ()
broadcastStateUpdate = do
  st <- getState
  for_ (Map.elems $ stPlayers st) $ \player ->
    case pConnection player of
      Nothing -> pure ()  -- can't update
      Just conn -> sendStateUpdate conn player st

extract :: Eq a => a -> [a] -> Maybe [a]
extract _ [] = Nothing
extract x (y : ys)
  | x == y    = Just ys
  | otherwise = (y:) <$> extract x ys

handle :: Api.Message_C2S -> Game ()
handle Api.Join{mcsPlayerName} = do
  st <- getState
  cookie <- getCookie
  connection <- getConnection

  -- check if this player already exists
  case [p | p <- Map.elems (stPlayers st), pName p == mcsPlayerName] of
    -- it is an existing player, take over session
    oldPlayer:_ -> do
      log $ show cookie ++ " resurrects player " ++ show (pCookie oldPlayer, pName oldPlayer)

      -- replace the player
      setState st
        { stPlayers = stPlayers st
          & Map.insert cookie oldPlayer
            { pCookie = cookie
            , pConnection = Just connection
            }
          & Map.delete (pCookie oldPlayer)
        }

      -- close the connection in case it's still alive
      case pConnection oldPlayer of
        Just conn -> close conn
        Nothing -> log $ "  -> old player already dead"

    _ -> do
      -- create a new player
      let (letters, rest) = splitAt 8 (stBag st)
      setState st
        { stPlayers = stPlayers st
          & Map.insert cookie Player
            { pName = mcsPlayerName
            , pConnection = Just connection
            , pLetters = letters
            , pScore = 0
            , pCookie = cookie
            }
          , stBag = rest
          }
      log $ show cookie ++ " is a new player"

  broadcastStateUpdate

handle Api.Drop{mcsI, mcsJ, mcsLetter} = do
  st <- getState
  player <- getPlayer

  let ij = (mcsI, mcsJ)
  case (Map.lookup ij $ stBoard st, extract mcsLetter $ pLetters player) of
    ( Just Api.Cell
      { Api.cBoost  = mbBoost
      , Api.cLetter = Nothing
      }
     , Just newLetters
     ) -> do
      let cell = Api.Cell
            { Api.cBoost  = mbBoost
            , Api.cLetter = Just mcsLetter
            }
      setState st
        { stBoard = Map.insert ij cell (stBoard st)
        , stPlayers = Map.insert (pCookie player) player{ pLetters = newLetters } (stPlayers st)
        }

    _ -> throwSoft "can't drop there"

  broadcastStateUpdate
