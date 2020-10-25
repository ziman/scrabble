module Component.Placeholder (new) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Effect.Class (class MonadEffect)

type Action = Void
type State = Unit
type Input = Unit
type Slots = ()

render :: forall m. MonadEffect m => String -> State -> H.ComponentHTML Action Slots m
render title _state =
  HH.div
    [HP.classes [HH.ClassName "placeholder", HH.ClassName title]]
    [HH.text title]

handleAction :: forall o m. Action -> H.HalogenM State Action Slots o m Unit
handleAction = absurd

new :: forall q o m. MonadEffect m => String -> H.Component HH.HTML q Input o m
new title = H.mkComponent
  { initialState: \_input -> unit
  , render: render title
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

-- vim: et ts=2 sts=2 sw=2
