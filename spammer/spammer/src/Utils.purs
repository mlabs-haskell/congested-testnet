module Spammer.Utils where

import Contract.Prelude

import Aeson (JsonDecodeError)
import Contract.Monad (Contract)
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

-- getBalance :: Contract BigInt
-- getBalance = do
--   mbalance <- getWalletBalance 
--   balance  <- liftMaybe (error "no utxos") mbalance 

