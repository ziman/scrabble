module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component.Game as Game

main :: Effect Unit
main = HA.runHalogenAff $
  HA.awaitBody >>=
    runUI Game.new unit
