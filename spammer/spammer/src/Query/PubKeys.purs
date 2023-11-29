module Spammer.Query.PubKeys where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), PubKeyHash(..))
import Contract.Monad (Contract, liftContractAffM)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Spammer.Db (executeQuery)
import Spammer.Keys (getEd25519HashFromPubKeyHex)
import Spammer.Utils (liftJsonDecodeError)

type Result = Array { pubkey :: String }

getPubKeyHash :: Contract PaymentPubKeyHash
getPubKeyHash = liftContractAffM
  "failed to get PublicKey for utxo out"
  do
    json <- executeQuery "SELECT pubkey FROM pkeys ORDER BY time ASC, balance DESC LIMIT 1;"
    result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
    let
      res = PaymentPubKeyHash
        <<< PubKeyHash
        <<< getEd25519HashFromPubKeyHex
        <<< _.pubkey
        <$> head result
    pure res

