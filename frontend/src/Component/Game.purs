module Component.Game (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import WebSocket as WS
import Effect (Effect)
import Effect.Exception (throwException)

import Api as Api
import Utils as Utils
import Component.Login as Login
import Component.Board as Board
import Component.Letters as Letters
import Component.PlayerList as PlayerList

type Props = Unit
data State
  = LoggedOut
  | LoggedIn Api.State

onMessage :: Self Props State -> WS.Capabilities Effect Api.Message_C2S -> Api.Message_S2C -> Effect Unit
onMessage self sock (Api.Update u) = do
  self.setState \s -> LoggedIn u.state

onMessage self sock msg = pure unit

render :: Self Props State -> JSX
render self =
  case self.state of
    LoggedOut ->
      R.div
      { className: "game"
      , children:
        [ Login.new
          { onSubmit: \playerName -> do
              Utils.log playerName
              WS.newWebSocket "ws://127.0.0.1:8083/" [] $
                WS.WebSocketsApp \env ->
                  { onopen: \sock ->
                      sock.send $ Api.Join {playerName}
                  , onmessage: \sock msg ->
                      onMessage self sock msg
                  , onerror: \err -> do
                      Utils.alert "connection error"
                      self.setState \s -> LoggedOut
                      throwException err
                  , onclose: \evt -> do
                      self.setState \s -> LoggedOut
                  }
          }
        ]
      }
    LoggedIn state ->
      R.div
      { className: "game"
      , children:
        [ PlayerList.new {players: state.players}
        , R.div
          { className: "main"
          , children:
            [ Board.new state.board
            , Letters.new {letters: state.letters}
            ]
          }
        ]
      }

new :: Props -> JSX
new = make (createComponent "Game")
  { initialState: LoggedOut
  , render
  }

-- vim: et ts=2 sts=2 sw=2
