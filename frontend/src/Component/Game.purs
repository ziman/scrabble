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

type WebSocket = WS.Capabilities Effect Api.Message_C2S 
type Props = Unit
data State
  = LoggedOut
  | LoggedIn WebSocket Api.State

onMessage :: Self Props State -> WebSocket -> Api.Message_S2C -> Effect Unit
onMessage self sock (Api.Update u) = do
  self.setState \s -> LoggedIn sock u.state

onMessage self sock (Api.Error e) = do
  Utils.alert e.message

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
    LoggedIn sock state ->
      R.div
      { className: "game"
      , children:
        [ PlayerList.new {players: state.players}
        , R.div
          { className: "main"
          , children:
            [ Board.new
              { board: state.board
              , onLetterDrop: \i j letter ->
                  sock.send $ Api.Drop {i, j, letter}
              }
            , Letters.new
              { letters: state.letters
              , onGetLetter:
                  sock.send $ Api.GetLetter
              }
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
