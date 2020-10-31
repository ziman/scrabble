module Component.VoteButtons (new) where

import Prelude
import Data.Foldable (sum)

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

import Api as Api

type Props =
  { onVote :: Boolean -> Effect Unit
  , vote :: Boolean
  , uncommittedWords :: Array Api.Word
  , bonus :: Int
  }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.div
  { className: "vote-buttons"
  , children:
    [ R.table
      { children:
        [ R.tbody
          { children:
            ( do
              word <- self.props.uncommittedWords
              pure $ R.tr
                { children:
                  [ R.td
                    { children: [R.text word.word]
                    }
                  , R.td
                    { children: [R.text $ show word.value]
                    }
                  ]
                }
            ) <> (
              if self.props.bonus > 0
                then
                  [ R.tr
                    { children:
                      [ R.td {children: [R.text "BONUS:"]}
                      , R.td {children: [R.text $ show self.props.bonus]}
                      ]
                    }
                  ]
                else
                  []
            ) <> [
              R.tr
              { children:
                [ R.td {children: [R.text "Total:"]}
                , R.td {children: [R.text $ show $
                    self.props.bonus + sum (self.props.uncommittedWords <#> \w -> w.value)]}
                ]
              }
            ]
          }
        ]
      }
    , R.div
      { children:
        [ R.button
          { children: [R.img {src: "thumbs-up.png"}]
          , onClick: capture_ $ self.props.onVote true
          , disabled: self.props.vote
          }
        , R.button
          { children: [R.img {src: "thumbs-down.png"}]
          , onClick: capture_ $ self.props.onVote false
          , disabled: not self.props.vote
          }
        ]
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "VoteButtons")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
