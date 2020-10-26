module Api where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Foreign.Object as Object
import Data.Argonaut.Core (caseJsonObject, fromString, fromObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Combinators ((.:))

type Letter =
  { letter :: String
  , value :: Int
  }

data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord

instance boostDecodeJson :: DecodeJson Boost where
  decodeJson json =
    -- decode via String
    decodeJson json >>= case _ of
      "DoubleLetter" -> pure DoubleLetter
      "TripleLetter" -> pure TripleLetter
      "DoubleWord" -> pure DoubleWord
      "TripleWord" -> pure TripleWord
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

type Cell =
  { boost :: Maybe Boost
  , letter :: Maybe Letter
  }

type Cookie = String

type Board =
  { cols :: Int
  , rows :: Int
  , cells :: Array (Array Cell)
  }

data Phase
  = WaitingForPlayers

instance phaseDecodeJson :: DecodeJson Phase where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "WaitingForPlayers" -> pure WaitingForPlayers
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

type State =
  { players :: Array String
  , board :: Board
  , letters :: Array Letter
  , name :: String
  , cookie :: String
  }

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }

instance msg_s2c_DecodeJson :: DecodeJson Message_S2C where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "Update" -> Update <$> decodeJson json
      "Error" -> Error <$> decodeJson json
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

data Message_C2S
  = Join { playerName :: String }
  | Drop { i :: Int, j :: Int, letter :: Letter }

instance msg_c2s_EncodeJson :: EncodeJson Message_C2S where
  encodeJson = case _ of
    Join obj -> "Join" // obj
    Drop obj -> "Drop" // obj

infix 3 addTag as //
addTag :: forall a. EncodeJson a => String -> a -> Json
addTag t obj = caseJsonObject json f json
  where
    json = encodeJson obj
    f obj' = fromObject $ Object.insert "tag" (fromString t) obj'
