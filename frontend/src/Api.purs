module Api where

import Data.Generic.Rep (class Generic)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)

type Letter =
  { letter :: String
  , value :: Int
  }
data Boost = DoubleLetter | TripleLetter | DoubleWord | TripleWord
data Cell = Blank | Boost Boost | Letter Letter
type Cookie = String

data Message_S2C
  = Update { clients :: Array String }
  | Error { message :: String }

derive instance genericMessage_S2C :: Generic Message_S2C _

instance msg_s2c_DecodeJson :: DecodeJson Message_S2C where
  decodeJson x = genericDecodeJson x  -- must be eta-long

data Message_C2S
  = Join { playerName :: String }

derive instance genericMessage_C2S :: Generic Message_C2S _

instance msg_c2s_EncodeJson :: EncodeJson Message_C2S where
  encodeJson x = genericEncodeJson x  -- must be eta-long
