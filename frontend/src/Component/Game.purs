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
import Component.Placeholder as UserList
import Component.Placeholder as Letters

type Props = Unit
data State
  = LoggedOut
  | LoggedIn String Api.Cookie

onMessage :: Self Props State -> WS.Capabilities Effect Api.Message_C2S -> Api.Message_S2C -> Effect Unit
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
    LoggedIn _playerName _cookie ->
      R.div
      { className: "game"
      , children:
        [ UserList.new {title: "user-list"}
        , R.div
          { className: "main"
          , children:
            [ Board.new unit
            , Letters.new {title: "letters"}
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
