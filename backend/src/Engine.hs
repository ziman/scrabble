{-# LANGUAGE AllowAmbiguousTypes #-}
module Engine (Game(..), runGame, HasError(..)) where

import System.Random
import Data.Functor

import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as Exception

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import qualified Game

class HasError msg_S2C where
  s2cError :: String -> msg_S2C

runEffect :: forall msg_S2C. (HasError msg_S2C, Aeson.ToJSON msg_S2C)
  => Game.Effect msg_S2C -> IO ()
runEffect = \case
  Game.Log msg -> putStrLn msg
  Game.Close conn -> closeConnection @msg_S2C conn
  Game.Send conn msg -> send conn msg

-- this function can only return hard errors
recv :: Aeson.FromJSON msg_C2S => WS.Connection -> IO (Either String msg_C2S)
recv connection = do
  msgBS <- (Right <$> WS.receiveData connection)
    `Exception.catch` \e -> pure $ Left (show (e :: WS.ConnectionException))
  pure $ case Aeson.decode <$> msgBS of
    Left err -> Left $ "can't recv: " ++ err
    Right Nothing -> Left $ "can't decode: " ++ show msgBS
    Right (Just msg) -> Right msg

send :: Aeson.ToJSON msg_S2C => WS.Connection -> msg_S2C -> IO ()
send connection msg = WS.sendTextData connection (Aeson.encode msg)

-- this function returns exactly on (hard) error
playerLoop
  :: forall msg_S2C msg_C2S
  .  ( Aeson.FromJSON msg_C2S
     , Aeson.ToJSON msg_S2C
     , HasError msg_S2C
     )
  => WS.Connection
  -> (msg_C2S -> IO (Either Game.Error ()))
  -> IO String
playerLoop connection handle =
  recv connection >>= \case
    Left err -> pure err  -- give up
    Right msg -> handle msg >>= \case
      Right () ->
        -- loop again
        playerLoop @msg_S2C connection handle

      Left (Game.SoftError msg) -> do
        -- send an error to the client but loop again
        send connection $ s2cError @msg_S2C msg
        playerLoop @msg_S2C connection handle

      Left (Game.HardError msg) ->
        -- give up
        pure msg

-- send close and receive all remaining messages
closeConnection :: forall msg_S2C. (Aeson.ToJSON msg_S2C, HasError msg_S2C) => WS.Connection -> IO ()
closeConnection conn =
  void $ forkIO $ do
    Exception.handle @SomeException (\_ -> pure ()) $ do
      WS.sendClose conn (Aeson.encode $ s2cError @msg_S2C "connection replaced")
      forever (void $ WS.receive conn)
    putStrLn $ "connection closed"

data Game st msg_C2S msg_S2C = Game
  { onMessage :: msg_C2S -> Game.GameM st msg_S2C ()
  , onDeadPlayer :: Game.GameM st msg_S2C ()
  }

application
  :: forall msg_S2C msg_C2S st
  .  ( Aeson.FromJSON msg_C2S
     , Aeson.ToJSON msg_S2C
     , HasError msg_S2C
     )
  => TVar st -> Game st msg_C2S msg_S2C -> WS.ServerApp
application tvState Game{onMessage,onDeadPlayer} pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    cookie <- randomIO
    let env = Game.Env{connection,cookie, state=tvState}

    -- run the player until dead
    result <- playerLoop @msg_S2C connection (Game.runGameM env runEffect . onMessage)
      `Exception.catch`
        \e -> pure $ show (e :: SomeException)

    -- print reason of death
    putStrLn $ "player " ++ show cookie ++ " dead: " ++ result

    -- mark player as dead
    void $ Game.runGameM env runEffect onDeadPlayer

runGame
  :: ( Aeson.FromJSON msg_C2S
     , Aeson.ToJSON msg_S2C
     , HasError msg_S2C
     )
  => String -> Int -> st -> Game st msg_C2S msg_S2C -> IO ()
runGame addr port initialState game = do
  tvState <- newTVarIO initialState
  WS.runServer addr port
    $ application tvState game
