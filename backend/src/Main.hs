module Main where

import System.Random
import Data.Functor
import Data.Foldable
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import qualified VectorShuffling.Immutable as Vec

import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS (evalRWST)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as Exception

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import qualified Api
import qualified Game

type Api = ReaderT Game.Env (ExceptT Game.Error IO)

throw :: Game.Error -> Api a
throw = lift . throwE

throwSoft :: String -> Api a
throwSoft = throw . Game.SoftError

throwHard :: String -> Api a
throwHard = throw . Game.HardError

recv :: Api Api.Message_C2S
recv = do
  conn <- Game.envConnection <$> ask
  msgBS <- liftIO $
    (Right <$> WS.receiveData conn)
      `Exception.catch` \e -> pure $ Left (show (e :: WS.ConnectionException))
  case Aeson.decode <$> msgBS of
    Left err -> throwHard $ "can't recv: " ++ err
    Right Nothing -> throwHard $ "can't decode: " ++ show msgBS
    Right (Just msg) -> pure msg

send :: Api.Message_S2C -> Api ()
send msg = do
  conn <- Game.envConnection <$> ask
  liftIO $ WS.sendTextData conn (Aeson.encode msg)

loop :: Api () -> Api a
loop api = do
  liftCatch catchE api $ \case
    Game.SoftError msg -> do
      send Api.Error{ mscMessage = msg }
      loop api
    Game.HardError msg -> throw $ Game.HardError msg
  loop api

getPlayer :: Api Game.Player
getPlayer = do
  cookie <- Game.envCookie <$> ask
  st <- liftIO . readTVarIO =<< (Game.envState <$> ask)
  case Map.lookup cookie (Game.stPlayers st) of
    Nothing -> throwHard "player not found"
    Just player -> pure player

playerLoop :: Api ()
playerLoop = loop (runGame . Game.handle =<< recv)

-- send close and receive all remaining messages
closeConnection :: WS.Connection -> IO ()
closeConnection conn =
  void $ forkIO $ do
    Exception.handle @SomeException (\_ -> pure ()) $ do
      WS.sendClose conn (Aeson.encode $ Api.Error "connection replaced")
      forever (void $ WS.receive conn)
    putStrLn $ "connection closed"

runEffect :: Game.Effect -> Api ()
runEffect = \case
  Game.Send conn msg ->
    liftIO $ WS.sendTextData conn (Aeson.encode msg)
  Game.Close conn ->
    liftIO $ closeConnection conn
  Game.Log msg ->
    liftIO $ putStrLn msg

runGame :: Game.Game a -> Api a
runGame game = do
  env <- ask
  liftIO (atomically $ runExceptT $ evalRWST game env ()) >>= \case
    Left err -> throw err
    Right (x, effects) -> do
      traverse_ runEffect effects
      pure x

application :: TVar Game.State -> WS.ServerApp
application tvState pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    cookie <- randomIO
    let env = Game.Env
          { envConnection = connection
          , envState      = tvState
          , envCookie     = cookie
          }

    result <-
      runExceptT (runReaderT playerLoop env)
        `Exception.catch`
          \e -> pure $ Left $ Game.HardError $ show (e :: SomeException)

    case result of
      Left err -> putStrLn $ "player " ++ show cookie ++ ": " ++ show err
      Right () -> putStrLn $ "player " ++ show cookie ++ " quit quietly"

    -- mark player as dead
    void $ runExceptT $ runReaderT (runGame Game.onDeadPlayer) env

-- repeats are fine
symmetry :: [(Int, Int)] -> [(Int, Int)]
symmetry ijs = concat
  [ [ (i, j)
    , (14-i, j)
    , (i, 14-j)
    , (14-i, 14-j)
    , (j, i)
    , (j, 14-i)
    , (14-j, i)
    , (14-j, 14-i)
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
  g <- newStdGen
  tvState <- newTVarIO $ initialState g
  WS.runServer "0.0.0.0" 8083
    $ application tvState
  where
    initialState g = Game.State
      { stPlayers = Map.empty
      , stBoardSize = (15, 15)
      , stBoard =
        Map.fromList
          [(ij, Api.Cell (Just boost) Nothing) | (boost, ijs) <- boosts, ij <- ijs]
        `Map.union`
          Map.fromList
            [((i,j), Api.Cell Nothing Nothing) | i <- [0..14], j <- [0..14]]
      , stBag = shuffle g $ concat
        [ replicate count (Api.Letter letter value)
        | (letter, value, count) <- lettersCZ
        ]
      }

    shuffle g = Vec.toList . fst . flip Vec.shuffle g . Vec.fromList

-- letter, value, count
lettersCZ :: [(Text, Int, Int)]
lettersCZ =
  [ ("A", 1, 6)
  , ("Á", 2, 2)
  , ("B", 2, 2)
  , ("C", 3, 2)
  , ("Č", 4, 2)
  , ("D", 2, 2)
  , ("Ď", 8, 1)
  , ("E", 1, 5)
  , ("É", 5, 1)
  , ("Ě", 5, 2)
  , ("F", 8, 1)
  , ("G", 8, 1)
  , ("H", 3, 2)
  , ("CH", 4, 2)
  , ("I", 1, 4)
  , ("Í", 2, 2)
  , ("J", 2, 2)
  , ("K", 1, 4)
  , ("L", 1, 4)
  , ("M", 2, 3)
  , ("N", 1, 3)
  , ("Ň", 6, 1)
  , ("O", 1, 6)
  , ("Ó", 10, 1)
  , ("P", 1, 3)
  , ("R", 1, 4)
  , ("Ř", 4, 2)
  , ("S", 1, 5)
  , ("Š", 3, 2)
  , ("T", 1, 4)
  , ("Ť", 6, 1)
  , ("U", 2, 3)
  , ("Ů", 5, 1)
  , ("Ú", 6, 1)
  , ("V", 1, 3)
  , ("X", 10, 2)
  , ("Y", 1, 3)
  , ("Ý", 4, 2)
  , ("Z", 3, 2)
  , ("Ž", 4, 2)
  , ("*", 0, 2)
  ]
