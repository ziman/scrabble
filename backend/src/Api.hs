module Api where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson.Casing
import qualified Data.Aeson as Aeson

data Letter = Letter
  { lLetter :: Text
  , lValue :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Letter where
  toJSON = Aeson.genericToJSON jsonOptions

data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Boost where
  toJSON = Aeson.genericToJSON jsonOptions

data Cell = Blank (Maybe Boost) | Played Letter
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Cell where
  toJSON = Aeson.genericToJSON jsonOptions

type Cookie = Text

data Board = Board
  { bCols :: Int
  , bRows :: Int
  , bCells :: [[Cell]]
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Board where
  toJSON = Aeson.genericToJSON jsonOptions

data Phase
  = WaitingForPlayers
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Phase where
  toJSON = Aeson.genericToJSON jsonOptions

data State = State
  { stPlayers :: [Text]
  , stBoard :: Board
  , stLetters :: [Letter]
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON State where
  toJSON = Aeson.genericToJSON jsonOptions

data Message_S2C
  = Error { mscMessage :: Text }
  | Update { mscState :: State }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Message_S2C where
  toJSON = Aeson.genericToJSON jsonOptions

data Message_C2S
  = Join { mcsPlayerName :: Text }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON Message_C2S where
  parseJSON = Aeson.genericParseJSON jsonOptions

jsonOptions :: Aeson.Options
jsonOptions = (aesonPrefix camelCase)
  { Aeson.sumEncoding = Aeson.defaultTaggedObject
  }

