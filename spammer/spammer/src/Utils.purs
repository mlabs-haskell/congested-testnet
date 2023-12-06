module Spammer.Utils where

import Contract.Prelude

import Aeson (JsonDecodeError)
import Contract.Monad (Contract)
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

decodeCborHexToBytes :: String -> Maybe ByteArray
decodeCborHexToBytes cborHex = do
  cborBa <- hexToByteArray cborHex
  hush $ toByteArray $ wrap $ wrap cborBa

