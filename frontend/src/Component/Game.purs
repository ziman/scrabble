module Component.Game (new) where

import Prelude

import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH

import Component.Placeholder as UserList
import Component.Placeholder as Board
import Component.Placeholder as Letters

import Effect.Class (class MonadEffect)

type Action = Void
type State = Unit
type Input = Unit
type Slots =
  ( userlist :: forall q. H.Slot q Void Int
  , board    :: forall q. H.Slot q Void Int
  , letters  :: forall q. H.Slot q Void Int
  )

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render state = HH.div_
  [ HH.slot (SProxy :: SProxy "userlist") 0 (UserList.new "user list") unit absurd
  , HH.div_
    [ HH.slot (SProxy :: SProxy "board") 1 (Board.new "board") unit absurd
    , HH.slot (SProxy :: SProxy "letters") 2 (Letters.new "letters") unit absurd
    ]
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action Slots o m Unit
handleAction = absurd

new :: forall q o m. MonadEffect m => H.Component HH.HTML q Input o m
new = H.mkComponent
  { initialState: \_input -> unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

-- vim: et ts=2 sts=2 sw=2
