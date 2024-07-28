module Spammer.Contracts.Faucet where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Data.BigInt as BInt
import Effect.Aff (error)


payToAddress :: String -> Contract TransactionHash
payToAddress pubKeyHashHex = do
  edHash <- liftM (error "can't convert hex to ed25519")  $ ed25519KeyHashFromBytes <<< hexToByteArrayUnsafe $ pubKeyHashHex
  let
    pkh = wrap $ wrap edHash
    constraints = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 1_000_000_000)
  submitTxFromConstraints mempty constraints

