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


type Result = Array { 
  pkey:: String,
  pubkey :: String
  }


getWallet' :: Aff KeyWallet 
getWallet' = do
  json <- executeQuery "SELECT pkey, pubkey FROM pkeys LIMIT 1;"
  result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
  res <- liftMaybe (error "empty array") $ head result 
  let 
      -- pkey = PrivatePaymentKey (getPrivateKeyFromHex res.pkey)
      pkey = PrivatePaymentKey (getPrivateKeyFromHex "f20eb3183f6d7cbb4e96ffd08a286392db1594ada6df1b731c9798af5edac754")
      keyWallet = privateKeysToKeyWallet pkey Nothing 
  pure keyWallet 

