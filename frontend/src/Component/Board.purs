module Component.Board (new) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Data.Array((..))

import Effect.Class (class MonadEffect)

type Action = Void
type State = Unit
type Input = Unit
type Slots = ()

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render _state =
  HH.table [HP.classes [HH.ClassName "board"]] $ do
    row <- 0..14
    pure $ HH.tr_ $ do
      col <-  0..14
      pure $ HH.td_ [HH.text "X"]

handleAction :: forall o m. Action -> H.HalogenM State Action Slots o m Unit
handleAction = absurd

new :: forall q o m. MonadEffect m => H.Component HH.HTML q Input o m
new = H.mkComponent
  { initialState: \_input -> unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

-- vim: et ts=2 sts=2 sw=2
