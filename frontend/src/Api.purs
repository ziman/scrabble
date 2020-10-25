module Api where

type Letter =
  { letter :: String
  , value :: Int
  }
data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord
data Cell = Blank | Boost Boost | Letter Letter
