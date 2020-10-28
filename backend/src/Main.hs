module Main where

import qualified Engine
import qualified Scrabble

main :: IO ()
main = do
  initialState <- Scrabble.mkInitialState
  Engine.runGame "0.0.0.0" 8083 initialState Scrabble.game
