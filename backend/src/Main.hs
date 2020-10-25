module Main where

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

type Letter = Text

data Client = Client
  { clName :: Text
  , clConnection :: WS.Connection
  , clLetters :: [Letter]
  , clScore :: Int
  }

data State = State
  { stClients :: [Client]
  , stBoardSize :: (Int, Int)
  , stBoard :: Map (Int, Int) Api.Cell
  , stBag :: [Letter]
  }

data Env = Env
  { envConnection :: WS.Connection
  , envState :: TVar State
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

clientLoop :: Api ()
clientLoop = do
  send $ Api.Update $ Api.State
    { Api.stPlayers = ["hello", "world"]
    , Api.stBoard = Api.Board
      { bRows = 15
      , bCols = 15
      , bCells = [[]]
      }
    , Api.stLetters = []
    }
  loop $ recv >>= handle

handle :: Api.Message_C2S -> Api ()
handle Api.Join{..} = do
  liftIO $ print mcsPlayerName

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
      { stClients = []
      , stBoardSize = (15, 15)
      , stBoard =
        Map.fromList
          [((i,j), Api.Blank Nothing) | i <- [0..14], j <- [0..14]]
        `Map.union`
          Map.fromList
            [(ij, Api.Blank $ Just boost) | (boost, ijs) <- boosts, ij <- ijs]
      , stBag = []
      }
