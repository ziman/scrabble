module Game
  ( Error(..)
  , Env(..)
  , Cookie
  , Effect(..)
  , GameM, runGameM
  , throw, throwSoft, throwHard
  , liftSTM
  , send, close
  , log
  , getCookie, getConnection
  , getState, setState, modifyState
  )
  where

import Prelude hiding (log)
import System.Random

import Data.Text (Text)
import Data.Foldable (traverse_)
import qualified Data.Text as Text

import Control.Exception (SomeException)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS hiding (state)
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

data Error
  = SoftError String  -- keep the connection
  | HardError String  -- kill the connection
  deriving (Eq, Ord)

instance Show Error where
  show (SoftError msg) = "soft error: " ++ msg
  show (HardError msg) = "hard error: " ++ msg

data Env st = Env
  { connection :: WS.Connection
  , state :: TVar st
  , cookie :: Cookie
  }

newtype Cookie = Cookie Text
  deriving newtype
    (Eq, Ord, Show, Aeson.ToJSON, Aeson.FromJSON)

-- this is a game; we don't care about the quality of the RNG too much
rstring :: RandomGen g => Int -> g -> (String, g)
rstring 0 g = ("", g)
rstring n g = (c:cs, g'')
  where
    (c, g') = randomR ('a', 'z') g
    (cs, g'') = rstring (n-1) g'

instance Random Cookie where
  randomR (_, _) = random
  random g = (Cookie (Text.pack rs), g')
    where
      (rs, g') = rstring 5 g

data Effect msg_S2C
  = Send WS.Connection msg_S2C
  | Close WS.Connection
  | Log String

type GameM st msg_S2C a = RWST (Env st) [Effect msg_S2C] () (ExceptT Error STM) a

throw :: Error -> GameM st msg_S2C a
throw = lift . throwE

throwSoft :: String -> GameM st msg_S2C a
throwSoft = throw . SoftError

throwHard :: String -> GameM st msg_S2C a
throwHard = throw . HardError

liftSTM :: STM a -> GameM st msg_S2C a
liftSTM = lift . lift

send :: Aeson.ToJSON msg_S2C => WS.Connection -> msg_S2C -> GameM st msg_S2C ()
send conn msg = tell [Send conn msg]

close :: WS.Connection -> GameM st msg_S2C ()
close conn = tell [Close conn]

log :: String -> GameM st msg_S2C ()
log msg = tell [Log msg]

readTVar :: TVar a -> GameM st msg_S2C a
readTVar = liftSTM . STM.readTVar

writeTVar :: TVar a -> a -> GameM st msg_S2C ()
writeTVar tv = liftSTM . STM.writeTVar tv

modifyTVar :: TVar a -> (a -> a) -> GameM st msg_S2C ()
modifyTVar tv = liftSTM . STM.modifyTVar tv

getCookie :: GameM st msg_S2C Cookie
getCookie = cookie <$> ask

getConnection :: GameM st msg_S2C WS.Connection
getConnection = connection <$> ask

getState :: GameM st msg_S2C st
getState = readTVar . state =<< ask

setState :: st -> GameM st msg_S2C ()
setState st = do
  tvState <- state <$> ask
  writeTVar tvState st

modifyState :: (st -> st) -> GameM st msg_S2C ()
modifyState f = do
  tvState <- state <$> ask
  modifyTVar tvState f

runGameM
  :: Aeson.ToJSON msg_S2C
  => Env st -> (Effect msg_S2C -> IO ()) -> GameM st msg_S2C a -> IO (Either Error a)
runGameM env runEffect game =
  (STM.atomically $ runExceptT $ evalRWST game env ()) >>= \case
    Left err -> pure (Left err)
    Right (x, effects) -> do
      (traverse_ runEffect effects *> pure (Right x))
        `Exception.catch`
          \e -> pure (Left $ HardError $ show (e :: SomeException))
