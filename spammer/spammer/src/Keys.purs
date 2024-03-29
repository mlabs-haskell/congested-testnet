module Spammer.Keys where

import Contract.Prelude
import Aeson (JsonDecodeError)
import Contract.Address (Ed25519KeyHash)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Data.Argonaut (decodeJson, parseJson)
import Data.String (drop)

foreign import getPrivateKeyFromHex :: CborHex -> PrivateKey
foreign import genPrivateKey :: Effect PrivateKey
foreign import getPrivateKeyHex :: PrivateKey -> CborHex
foreign import getPubKeyHex :: PrivateKey -> CborHex
foreign import getEd25519HashFromPubKeyHex :: CborHex -> Ed25519KeyHash
foreign import getPubKeyHashHex :: PrivateKey -> CborHex
foreign import getEdHash :: PrivateKey -> Ed25519KeyHash
foreign import getHexFromEd25519Hash :: Ed25519KeyHash -> CborHex
foreign import getEd25519HashFromPubKeyHexEffect :: CborHex -> Effect Ed25519KeyHash

-- | Typical key file format  
type KeyFile =
  { type :: String
  , description :: String
  , cborHex :: String
  }

type CborHex = String

parseKeyFromJsonString :: String -> Either JsonDecodeError CborHex
parseKeyFromJsonString jsonString = do
  keyFile :: KeyFile <- parseJson jsonString >>= decodeJson
  let
    cborHex = keyFile.cborHex
    cborHexWithoutHeader = drop 4 cborHex
  pure cborHexWithoutHeader
