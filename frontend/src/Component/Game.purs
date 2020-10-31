module Component.Game (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.Location as Location

import WebSocket as WS
import Effect (Effect)
import Effect.Exception (throwException)

import Api as Api
import Utils as Utils
import Component.Login as Login
import Component.Board as Board
import Component.Letters as Letters
import Component.PlayerList as PlayerList
import Component.VoteButtons as VoteButtons

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

getWsUrl :: Effect String
getWsUrl = do
  l <- Window.location =<< HTML.window
  p <- Location.protocol l
  h <- Location.host l
  pure $ (if p == "https:" then "wss://" else "ws://") <> h <> "/ws"

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
              wsUrl <- getWsUrl
              WS.newWebSocket wsUrl [] $
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
        [ R.div
          { className: "left-column"
          , children:
            [ PlayerList.new {players: state.players}
            , R.div
              { children:
                [ R.text $ "Left: " <> show state.lettersLeft
                ]
              }
            , VoteButtons.new
              { onVote: \vote -> sock.send (Api.Vote {vote})
              , vote: state.vote
              , uncommittedWords: state.uncommittedWords
              , bonus: state.bonus
              }
            ]
          }
        , R.div
          { className: "main"
          , children:
            [ Board.new
              { board: state.board
              , uncommitted: state.uncommitted
              , onLetterDrop: \src dst ->
                  sock.send $ Api.Drop {src, dst}
              }
            , R.div
              { className: "bottom-bar"
              , children:
                [ Letters.new
                  { letters: state.letters
                  , onGetLetter:
                      sock.send $ Api.GetLetter
                  , onLetterDrop: \src dst ->
                      sock.send $ Api.Drop {src, dst}
                  }
                ]
              }
            , R.p {children: [R.text "svetlocervena: 2x pismeno, tmavocervena: 3x pismeno"]}
            , R.p {children: [R.text "svetlozelena: 2x slovo, tmavozelena: 3x slovo"]}
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
