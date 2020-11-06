module Component.Letters (new) where

import Prelude
import Data.Array (length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

import Api as Api
import Utils as Utils
import Component.Letter as Letter

type Props =
  { letters :: Array Api.Letter
  , onLetterDrop :: Api.LetterSpot -> Api.LetterSpot -> Effect Unit
  , onRecycling :: Effect Unit
  }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.ul
  { className: "letters"
  , children:
    [ R.li
      { children:
        [ R.button
          { children:
            [ R.img {src: "/recycling.png"}
            ]
          , onClick: capture_ self.props.onRecycling
          }
        ]
      , className: "recycle-letters"
      }
    ] <> (do
      Tuple i letter <- Utils.enumerate self.props.letters
      pure $ R.li
        { children:
          [ Letter.new
            { letter
            , spot: Just (Api.Letters i)
            , isUncommitted: false
            }
          ]
        , onDragOver: Utils.acceptDrop
        , onDragEnter: Utils.acceptDrop
        , onDrop: Utils.dropHandler \srcSpot ->
            self.props.onLetterDrop srcSpot (Api.Letters i)
        }
    )
  , onDragOver: Utils.acceptDrop
  , onDragEnter: Utils.acceptDrop
  , onDrop: Utils.dropHandler \srcSpot ->
      self.props.onLetterDrop srcSpot (Api.Letters $ length self.props.letters)
  }

new :: Props -> JSX
new = make (createComponent "Letters")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
