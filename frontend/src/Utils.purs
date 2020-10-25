module Utils where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Web.HTML as Html
import Web.HTML.Window as Window

alert :: forall m. MonadEffect m => String -> m Unit
alert msg = liftEffect (Window.alert msg =<< Html.window)

log :: forall m. MonadEffect m => String -> m Unit
log msg = liftEffect $ Console.log msg
