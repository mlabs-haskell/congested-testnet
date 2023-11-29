module Spammer.Contracts where

import Contract.Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.ScriptLookups (unspentOutputs)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (getLovelace, lovelaceValueOf, valueToCoin)
import Contract.Wallet (KeyWallet, getWalletBalance, getWalletUtxos, ownPaymentPubKeyHash, ownStakePubKeyHash, withKeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Data.BigInt as BInt
import Data.BigNumber (fromNumber, fromInt)
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.PubKeys (getPubKeyHash)
import Spammer.Query.Wallet (countNullWallets, generateNewWalletDb, getWallet', updateWalletBalanceDb)

loopPayWalletToPubKey :: Contract Unit
loopPayWalletToPubKey = do
  keyWallet <- getWallet'
  pubKeyHashToPay <- getPubKeyHash
  res <- try do
    payFromWalletToPubKey keyWallet pubKeyHashToPay
  case res of
    Left _ -> do
      generateNewWalletDb
      loopPayWalletToPubKey
    Right _ -> do loopPayWalletToPubKey

payFromWalletToPubKey :: KeyWallet -> PaymentPubKeyHash -> Contract Unit
payFromWalletToPubKey wallet pubKeyHash = withKeyWallet wallet do
  mUtxos <- getWalletUtxos
  utxos <- liftMaybe (error "no utxos") mUtxos
  mbalance <- getWalletBalance
  balance <- liftMaybe (error "no utxos") mbalance
  mownPubKeyHash <- ownPaymentPubKeyHash
  ownPubKeyHash <- liftMaybe (error "no own pubkeyhash") mownPubKeyHash
  let
    keyWalletLovelace = getLovelace <<< valueToCoin $ balance
  let
    value = lovelaceValueOf (keyWalletLovelace / (BInt.fromInt 2))
    lookups = unspentOutputs utxos
    constraints = mustPayToPubKey pubKeyHash value
  updateWalletBalanceDb ownPubKeyHash keyWalletLovelace
  txhash <- submitTxFromConstraints lookups constraints
  -- awaitTxConfirmed txhash
  pure unit

