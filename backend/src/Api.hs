module Api where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)

import Game.WSGame.Engine (HasError(..))

data Letter = Letter
  { letter :: Text
  , value :: Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Cell = Cell
  { boost :: Maybe Boost
  , letter :: Maybe Letter
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Board = MkBoard
  { cols :: Int
  , rows :: Int
  , cells :: [[Cell]]
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
  , vote :: Bool
  , uncommitted :: [(Int, Int)]
  , uncommittedWords :: [UncommittedWord]
  , bonus :: Int
  , lettersLeft :: Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance HasError Message_S2C where
  s2cError = Error

data LetterSpot
  = Board { i :: Int, j :: Int }
  | Letters { idx :: Int }
  deriving (Eq, Ord, Show, Generic, FromJSON)

data Message_C2S
  = Join { playerName :: Text }
  | Drop { src :: LetterSpot, dst :: LetterSpot }
  | Vote { vote :: Bool }
  deriving (Eq, Ord, Show, Generic, FromJSON)
