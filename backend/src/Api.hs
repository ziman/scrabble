module Api where

import GHC.Generics
import System.Random
import Data.Text (Text)
import Data.Aeson.Casing
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

data Letter = Letter
  { lLetter :: Text
  , lValue :: Int
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
  { cBoost :: Maybe Boost
  , cLetter :: Maybe Letter
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Cell where
  toJSON = Aeson.genericToJSON jsonOptions

newtype Cookie = Cookie { unCookie :: Text }
  deriving newtype
    (Eq, Ord, Show, Aeson.ToJSON, Aeson.FromJSON)

-- this is a game; we don't care about the quality of the RNG too much
rstring :: RandomGen g => Int -> g -> (String, g)
rstring 0 g = ("", g)
rstring n g = (c:cs, g'')
  where
    (c, g') = randomR ('a', 'z') g
    (cs, g'') = rstring (n-1) g'

instance Random Cookie where
  randomR (_, _) = random
  random g = (Cookie (Text.pack rs), g')
    where
      (rs, g') = rstring 5 g

data Board = MkBoard
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

data Player = Player
  { pName    :: Text
  , pLetters :: Int
  , pScore   :: Int
  , pIsAlive :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Player where
  toJSON = Aeson.genericToJSON jsonOptions

data State = State
  { stPlayers :: [Player]
  , stBoard :: Board
  , stLetters :: [Letter]
  , stName :: Text
  , stCookie :: Cookie
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON State where
  toJSON = Aeson.genericToJSON jsonOptions

data Message_S2C
  = Error { mscMessage :: String }
  | Update { mscState :: State }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Message_S2C where
  toJSON = Aeson.genericToJSON jsonOptions

data LetterSpot
  = Board { lsI :: Int, lsJ :: Int }
  | Letters { lsIdx :: Int }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON LetterSpot where
  parseJSON = Aeson.genericParseJSON jsonOptions

data Message_C2S
  = Join { mcsPlayerName :: Text }
  | Drop { mcsSrc :: LetterSpot, mcsDst :: LetterSpot }
  | GetLetter
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON Message_C2S where
  parseJSON = Aeson.genericParseJSON jsonOptions

jsonOptions :: Aeson.Options
jsonOptions = (aesonPrefix camelCase)
  { Aeson.sumEncoding = Aeson.defaultTaggedObject
  }

