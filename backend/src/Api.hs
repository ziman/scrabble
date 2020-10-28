module Api where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Aeson as Aeson

import Game
import Engine

data Letter = Letter
  { letter :: Text
  , value :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Letter where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON Letter where
  parseJSON = Aeson.genericParseJSON jsonOptions

data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Boost where
  toJSON = Aeson.genericToJSON jsonOptions

data Cell = Cell
  { boost :: Maybe Boost
  , letter :: Maybe Letter
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Cell where
  toJSON = Aeson.genericToJSON jsonOptions

data Board = MkBoard
  { cols :: Int
  , rows :: Int
  , cells :: [[Cell]]
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Board where
  toJSON = Aeson.genericToJSON jsonOptions

data Phase
  = WaitingForPlayers
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Phase where
  toJSON = Aeson.genericToJSON jsonOptions

data Player = Player
  { name    :: Text
  , letters :: Int
  , score   :: Int
  , isAlive :: Bool
  , vote    :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Player where
  toJSON = Aeson.genericToJSON jsonOptions

data State = State
  { players :: [Player]
  , board :: Board
  , letters :: [Letter]
  , name :: Text
  , cookie :: Cookie
  , vote :: Bool
  , uncommitted :: [(Int, Int)]
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON State where
  toJSON = Aeson.genericToJSON jsonOptions

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Message_S2C where
  toJSON = Aeson.genericToJSON jsonOptions

instance Engine.HasError Message_S2C where
  s2cError = Error

data LetterSpot
  = Board { i :: Int, j :: Int }
  | Letters { idx :: Int }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON LetterSpot where
  parseJSON = Aeson.genericParseJSON jsonOptions

data Message_C2S
  = Join { playerName :: Text }
  | Drop { src :: LetterSpot, dst :: LetterSpot }
  | GetLetter
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON Message_C2S where
  parseJSON = Aeson.genericParseJSON jsonOptions

jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions
