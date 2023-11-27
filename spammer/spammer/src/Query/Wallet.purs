module Spammer.Query.Wallet where

import Contract.Prelude

import Contract.Wallet (KeyWallet, PrivatePaymentKey(..), privateKeysToKeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Effect.Exception (error)
import Spammer.Db (executeQuery)
import Spammer.Keys (getPrivateKeyFromHex)
import Spammer.Utils (liftJsonDecodeError)

type Result = Array
  { pkey :: String
  }

getWallet' :: Aff KeyWallet
getWallet' = do
  json <- executeQuery "SELECT pkey FROM pkeys LIMIT 1;"
  result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
  res <- liftMaybe (error "empty array in getWallet'") $ head result
  let
    pkey = PrivatePaymentKey (getPrivateKeyFromHex res.pkey)
    keyWallet = privateKeysToKeyWallet pkey Nothing
  pure keyWallet

