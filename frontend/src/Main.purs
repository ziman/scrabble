module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Component.Game as Game

main :: Effect Unit
main =
  map toNonElementParentNode (window >>= document)
  >>= getElementById "container"
  >>= case _ of
    Nothing -> throw "Container element not found."
    Just c ->
      let app = Game.new unit
       in render app c

-- vim: et ts=2 sts=2 sw=2
