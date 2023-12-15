module Spammer.Query.Utils where

import Contract.Prelude

import Aeson (JsonDecodeError)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Scripts (Validator(..))
import Contract.TextEnvelope (TextEnvelopeType(..), plutusScriptV2FromEnvelope)
import Ctl.Internal.Types.ByteArray (ByteArray, hexToByteArray)
import Ctl.Internal.Types.Cbor (toByteArray)
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.BigInt (BigInt)
import Effect.Exception (throw)

liftJsonDecodeError :: forall a. DecodeJson a => Either JsonDecodeError a -> Effect a
liftJsonDecodeError eitherErrA = do
  case eitherErrA of
    Left e -> throw $ printJsonDecodeError e
    Right x -> pure x

quotes :: String -> String
quotes x = "'" <> x <> "'"

commas :: String -> String
commas x = "," <> x <> ","

bytea :: String -> String
bytea x = "decode( " <> quotes x <> ",'hex')"

decodeCborHexToBytes :: String -> Maybe ByteArray
decodeCborHexToBytes cborHex = do
  cborBa <- hexToByteArray cborHex
  hush $ toByteArray $ wrap $ wrap cborBa

decodeAikenHexToScriptHex :: String -> Maybe String
decodeAikenHexToScriptHex aikenHex = do
  bytes <- decodeCborHexToBytes aikenHex
  pure <<< byteArrayToHex $ bytes

