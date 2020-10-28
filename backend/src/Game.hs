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
import Control.Monad.Trans.RWS.CPS hiding (state)
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM

import qualified Network.WebSockets as WS

import qualified Api

data Player = Player
  { name :: Text
  , connection :: Maybe WS.Connection
  , letters :: [Api.Letter]
  , score :: Int
  , cookie :: Api.Cookie
  , vote :: Maybe Bool
  }

instance Show Player where
  show Player{name,cookie,score,letters}
    = show (name, cookie, score, letters)

data State = State
  { players :: Map Api.Cookie Player
  , boardSize :: (Int, Int)
  , board :: Map (Int, Int) Api.Cell
  , bag :: [Api.Letter]
  , uncommitted :: Set (Int, Int)
  }
  deriving Show

data Env = Env
  { connection :: WS.Connection
  , state :: TVar State
  , cookie :: Api.Cookie
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

the :: (a -> b) -> a -> b
the proj = proj

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
getCookie = the @Env cookie <$> ask

getConnection :: Game WS.Connection
getConnection = the @Env connection <$> ask

getState :: Game State
getState = readTVar . state =<< ask

setState :: State -> Game ()
setState st = do
  tvState <- state <$> ask
  writeTVar tvState st

modifyState :: (State -> State) -> Game ()
modifyState f = do
  tvState <- state <$> ask
  modifyTVar tvState f

getPlayer :: Game Player
getPlayer = do
  st <- getState
  cookie <- the @Env cookie <$> ask
  case Map.lookup cookie (players st) of
    Nothing -> throwHard $ "could not resolve cookie " ++ show cookie
    Just player -> pure player

onDeadPlayer :: Game ()
onDeadPlayer = do
  player@Player{cookie} <- getPlayer
  modifyState $ \st -> st
    { players =
        Map.insert
          cookie
          (player{connection = Nothing} :: Player)
          (players st)
    }
  broadcastStateUpdate

sendStateUpdate :: WS.Connection -> Player -> State -> Game ()
sendStateUpdate conn Player{..} st = do
  let (rows, cols) = boardSize st
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = name
        , score   = score
        , letters = length letters
        , isAlive = isJust connection
        , vote    = vote
        }
      | Player{..} <- Map.elems (players st)
      ]
    , board = Api.MkBoard  -- we could precompute this
      { rows = rows
      , cols = cols
      , cells =
        [ [ board st Map.! (i, j)
          | j <- [0..cols-1]
          ]
        | i <- [0..rows-1]
        ]
      }
    , letters = letters
    , name    = name
    , cookie  = cookie
    , uncommitted = Set.toList (uncommitted st)
    }

broadcastStateUpdate :: Game ()
broadcastStateUpdate = do
  st <- getState
  for_ (Map.elems $ players st) $ \player@Player{connection} ->
    case connection of
      Nothing -> pure ()  -- can't update
      Just conn -> sendStateUpdate conn player st

resetVotes :: Game ()
resetVotes = modifyState $ \st -> st
  { players = players st
    & Map.map (\p -> p{ vote = Nothing })
  }

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
handle Api.Join{playerName} = do
  st <- getState
  thisCookie <- getCookie
  thisConnection <- getConnection

  -- check if this player already exists
  case [p | p <- Map.elems (players st), name p == playerName] of
    -- it is an existing player, take over session
    oldPlayer@Player{cookie=oldCookie, connection=oldConnection}:_ -> do
      log $ show thisCookie ++ " resurrects player " ++ show (oldCookie, name oldPlayer)

      -- replace the player
      setState st
        { players = players st
          & Map.insert thisCookie (oldPlayer
            { cookie = thisCookie
            , connection = Just thisConnection
            } :: Player)
          & Map.delete oldCookie
        }

      -- close the connection in case it's still alive
      case oldConnection of
        Just conn -> close conn
        Nothing -> log $ "  -> old player already dead"

    _ -> do
      -- create a new player
      let (letters, rest) = splitAt 8 (bag st)
      setState st
        { players = players st
          & Map.insert thisCookie Player
            { name = playerName
            , connection = Just thisConnection
            , letters = letters
            , score = 0
            , cookie = thisCookie
            , vote = Nothing
            }
          , bag = rest
          }
      log $ show thisCookie ++ " is a new player"

  broadcastStateUpdate

handle Api.Drop{src, dst} = do
  st <- getState
  player@Player{cookie} <- getPlayer

  case (src, dst) of
    (Api.Letters k, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just (letter, rest) <- extract k (letters player)
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
          , players = players st
            & Map.insert cookie player{letters = rest}
          , uncommitted = uncommitted st
            & Set.insert (dstI, dstJ)
          }

        resetVotes
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted st
            & Set.delete (srcI, srcJ)
            & Set.insert (dstI, dstJ)
          }

        resetVotes
        broadcastStateUpdate

    (Api.Letters srcIdx, Api.Letters dstIdx)
      | moved <- move srcIdx dstIdx (letters player)
      -> do
        setState st
          { players = players st
            & Map.insert cookie player{letters = moved}
          }
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Letters dstIdx)
      | Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      -> do
        let (ls, rs) = splitAt dstIdx (letters player)
        setState st
          { players = players st
            & Map.insert cookie player{letters = ls ++ letter : rs}
          , board = board st
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted st
            & Set.delete (srcI, srcJ)
          }
        broadcastStateUpdate

    _ -> throwSoft "can't drop there"


handle Api.GetLetter = do
  st <- getState
  player@Player{cookie} <- getPlayer

  case bag st of
    [] -> throwSoft "no more letters"
    l:ls -> do
      setState st
        { bag = ls
        , players =
            Map.insert
              cookie
              player{ letters = letters player ++ [l] }
              (players st)
        }
      broadcastStateUpdate
