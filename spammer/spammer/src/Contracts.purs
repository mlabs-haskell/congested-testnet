module Spammer.Contracts where

import Contract.Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.ScriptLookups (unspentOutputs)
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, withKeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Data.BigInt as BInt
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.PubKeys (getPubKeyHash)
import Spammer.Query.Wallet (getWallet')

loopPayWalletFromPubKey :: Maybe KeyWallet -> Contract Unit
loopPayWalletFromPubKey previousWalletWithError = do
  keyWallet <- getWallet' previousWalletWithError
  pubKeyHashToPay <- getPubKeyHash
  res <- try $ payFromWalletToPubKey keyWallet pubKeyHashToPay
  case res of
    Left _ -> loopPayWalletFromPubKey (Just keyWallet)
    Right _ -> loopPayWalletFromPubKey Nothing

payFromWalletToPubKey :: KeyWallet -> PaymentPubKeyHash -> Contract Unit
payFromWalletToPubKey wallet pubKeyHash = withKeyWallet wallet do
  mUtxos <- getWalletUtxos
  utxos <- liftMaybe (error "no utxos") mUtxos
  let
    value = lovelaceValueOf (BInt.fromInt 2_000_000)
    lookups = unspentOutputs utxos
    constraints = mustPayToPubKey pubKeyHash value
  _ <- submitTxFromConstraints lookups constraints
  pure unit

