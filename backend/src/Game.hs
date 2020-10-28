module Game where

import Prelude hiding (log)

import Data.Functor
import Data.Function
import Data.Foldable
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

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
  , pVote :: Maybe Bool
  }

instance Show Player where
  show c = show (pName c, pCookie c, pScore c, pLetters c)

data State = State
  { stPlayers :: Map Api.Cookie Player
  , stBoardSize :: (Int, Int)
  , stBoard :: Map (Int, Int) Api.Cell
  , stBag :: [Api.Letter]
  , stUncommitted :: Set (Int, Int)
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

onDeadPlayer :: Game ()
onDeadPlayer = do
  player <- getPlayer
  modifyState $ \st -> st
    { stPlayers =
        Map.insert
          (pCookie player)
          player{ pConnection = Nothing }
          (stPlayers st)
    }
  broadcastStateUpdate

sendStateUpdate :: WS.Connection -> Player -> State -> Game ()
sendStateUpdate conn player st = do
  let (rows, cols) = stBoardSize st
  send conn $ Api.Update $ Api.State
    { stPlayers =
      [ Api.Player
        { pName    = pName p
        , pScore   = pScore p
        , pLetters = length (pLetters p)
        , pIsAlive = isJust (pConnection p)
        , pVote = pVote p
        }
      | p <- Map.elems (stPlayers st)
      ]
    , stBoard = Api.MkBoard  -- we could precompute this
      { bRows = rows
      , bCols = cols
      , bCells =
        [ [ stBoard st Map.! (i, j)
          | j <- [0..cols-1]
          ]
        | i <- [0..rows-1]
        ]
      }
    , stLetters = pLetters player
    , stName    = pName player
    , stCookie  = pCookie player
    , stUncommitted = Set.toList (stUncommitted st)
    }

broadcastStateUpdate :: Game ()
broadcastStateUpdate = do
  st <- getState
  for_ (Map.elems $ stPlayers st) $ \player ->
    case pConnection player of
      Nothing -> pure ()  -- can't update
      Just conn -> sendStateUpdate conn player st

extract :: Int -> [a] -> Maybe (a, [a])
extract _ [] = Nothing
extract 0 (x : xs) = Just (x, xs)
extract i (x : xs) =
  extract (i-1) xs <&>
    \(y, ys) -> (y, x:ys)

idx :: Int -> [a] -> Maybe a
idx _ [] = Nothing
idx 0 (x:_) = Just x
idx i (_:xs) = idx (i-1) xs

move :: Int -> Int -> [a] -> [a]
move i j xs
  | i < j
  , (xsA, xsBC) <- splitAt i xs
  , (b:xsB, xsC) <- splitAt (j-i+1) xsBC
  = xsA ++ xsB ++ b : xsC

  | i > j
  , (xsA, xsBC) <- splitAt j xs
  , (xsB, c:xsC) <- splitAt (i-j) xsBC
  = xsA ++ c : xsB ++ xsC

  | otherwise = xs

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
            , pVote = Nothing
            }
          , stBag = rest
          }
      log $ show cookie ++ " is a new player"

  broadcastStateUpdate

handle Api.Drop{mcsSrc, mcsDst} = do
  st <- getState
  player <- getPlayer

  case (mcsSrc, mcsDst) of
    (Api.Letters k, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{cLetter = Nothing}
          <- Map.lookup (dstI, dstJ) (stBoard st)
      , Just (letter, rest) <- extract k (pLetters player)
      -> do
        setState st
          { stBoard = stBoard st
            & Map.insert (dstI, dstJ) cellDst{Api.cLetter = Just letter}
          , stPlayers = stPlayers st
            & Map.insert (pCookie player) player{pLetters = rest}
          }

        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{cLetter = Nothing}
          <- Map.lookup (dstI, dstJ) (stBoard st)
      , Just cellSrc@Api.Cell{cLetter = Just letter}
          <- Map.lookup (srcI, srcJ) (stBoard st)
      -> do
        setState st
          { stBoard = stBoard st
            & Map.insert (dstI, dstJ) cellDst{Api.cLetter = Just letter}
            & Map.insert (srcI, srcJ) cellSrc{Api.cLetter = Nothing}
          }
        broadcastStateUpdate

    (Api.Letters src, Api.Letters dst)
      | moved <- move src dst (pLetters player)
      -> do
        setState st
          { stPlayers = stPlayers st
            & Map.insert (pCookie player) player{pLetters = moved}
          }
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Letters dst)
      | Just cellSrc@Api.Cell{cLetter = Just letter}
          <- Map.lookup (srcI, srcJ) (stBoard st)
      -> do
        let (ls, rs) = splitAt dst (pLetters player)
        setState st
          { stPlayers = stPlayers st
            & Map.insert (pCookie player) player{pLetters = ls ++ letter : rs}
          , stBoard = stBoard st
            & Map.insert (srcI, srcJ) cellSrc{Api.cLetter = Nothing}
          }
        broadcastStateUpdate

    _ -> throwSoft "can't drop there"


handle Api.GetLetter = do
  st <- getState
  player <- getPlayer

  case stBag st of
    [] -> throwSoft "no more letters"
    l:ls -> do
      setState st
        { stBag = ls
        , stPlayers =
            Map.insert
              (pCookie player)
              player{ pLetters = pLetters player ++ [l] }
              (stPlayers st)
        }
      broadcastStateUpdate
