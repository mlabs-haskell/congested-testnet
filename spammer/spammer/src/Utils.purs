module Spammer.Utils where

import Contract.Prelude
import Aeson (JsonDecodeError)
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Effect.Exception (throw)

liftJsonDecodeError :: forall a. DecodeJson a => Either JsonDecodeError a -> Effect a
liftJsonDecodeError eitherErrA = do
  case eitherErrA of
    Left e -> throw $ printJsonDecodeError e
    Right x -> pure x

quotes :: String -> String
quotes x = "'" <> x <> "'"
