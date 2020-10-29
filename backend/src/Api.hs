module Api where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)

import Game
import Engine

data Letter = Letter
  { letter :: Text
  , value :: Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Cell = Cell
  { boost :: Maybe Boost
  , letter :: Maybe Letter
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Board = MkBoard
  { cols :: Int
  , rows :: Int
  , cells :: [[Cell]]
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Phase
  = WaitingForPlayers
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Player = Player
  { name    :: Text
  , letters :: Int
  , score   :: Int
  , isAlive :: Bool
  , vote    :: Bool
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data UncommittedWord = UncommittedWord
  { word :: Text
  , value :: Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data State = State
  { players :: [Player]
  , board :: Board
  , letters :: [Letter]
  , name :: Text
  , cookie :: Cookie
  , vote :: Bool
  , uncommitted :: [(Int, Int)]
  , uncommittedWords :: [UncommittedWord]
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Engine.HasError Message_S2C where
  s2cError = Error

data LetterSpot
  = Board { i :: Int, j :: Int }
  | Letters { idx :: Int }
  deriving (Eq, Ord, Show, Generic, FromJSON)

data Message_C2S
  = Join { playerName :: Text }
  | Drop { src :: LetterSpot, dst :: LetterSpot }
  | GetLetter
  | Vote { vote :: Bool }
  deriving (Eq, Ord, Show, Generic, FromJSON)
