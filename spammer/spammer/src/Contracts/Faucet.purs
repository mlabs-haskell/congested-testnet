module Spammer.Contracts.Faucet where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (TransactionHash, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Data.BigInt as BInt
import Data.String as String
import Spammer.Keys (getEd25519HashFromPubKeyHexEffect)



payToAddress :: String -> Contract TransactionHash 
payToAddress pubKeyHex = do
  edHash <- liftEffect $ getEd25519HashFromPubKeyHexEffect $ (String.drop 4 pubKeyHex)
  let
      pkh = wrap $ wrap edHash
      constraints = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 1_000_000_000) 
  submitTxFromConstraints mempty constraints






