module Spammer.Contracts where

import Contract.Prelude

import Contract.Address (NetworkId(..), PaymentPubKey(..), scriptHashAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, ownPaymentPubKeyHash, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToPubKey, mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (Wallet, getWalletUtxos, ownStakePubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Contract.Wallet (withWallet)
import Data.BigInt as BInt
import Data.Map (findMax)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)


payFromWalletToPubKey :: Wallet -> PaymentPubKey -> Contract Unit 
payFromWalletToPubKey wallet pubKey = withWallet \wallet -> do
  mUtxos <- getWalletUtxos 
  keyHashes <- ownStakePubKeyHashes 
  utxos <- liftMaybe (error "no utxos") mUtxos
  log $ show $ keyHashes
  -- let
  --     value = lovelaceValueOf (BInt.fromInt 2_000_000) 
  --     lookups = unspentOutputs utxos 
  --     constraints = mustPayToPubKey (validatorHash val) unitDatum DatumWitness value 
  -- txId <- submitTxFromConstraints lookups constraints
  -- log $ show $ txId 
  log "Successfully submitted"

