module Component.Letter (new) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Effect.Class (class MonadEffect)

type Action = Void
type State = Unit
type Input = String
type Slots = ()
type Env = {letter :: String, value :: Int}

render :: forall m. MonadEffect m => Env -> State -> H.ComponentHTML Action Slots m
render env _state =
  HH.span
    [HP.classes [HH.ClassName "letter"]]
    [ HH.text env.letter
    , HH.span
      [HP.classes [HH.ClassName "value"]]
      [HH.text $ show env.value]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action Slots o m Unit
handleAction = absurd

new :: forall q o m. MonadEffect m => Env -> H.Component HH.HTML q Input o m
new env = H.mkComponent
  { initialState: \_input -> unit
  , render: render env
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

-- vim: et ts=2 sts=2 sw=2
