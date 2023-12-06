module Spammer.Contracts.Lock where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, plutusV1Script, plutusV2Script, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.BigInt as BInt
import Data.Map (findMax)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Spammer.Utils (decodeCborHexToBytes)

s = "5850584e0100003222253330043253330055333005533300553330054a229445280a5114a0294452808008a50323300100148000894ccc018cdc42405000229444cc008008cdc0000a400429309b2b2b9a01"

getValidator :: Contract Validator
getValidator = do
  arr <- liftMaybe (error "no parse") (decodeCborHexToBytes s)
  pure $ wrap <<< plutusV1Script $ arr

lock :: Contract Unit
lock = do
  val <- getValidator
  mUtxos <- getWalletUtxos
  utxos <- liftMaybe (error "no utxos") mUtxos
  let
    value = lovelaceValueOf (BInt.fromInt 2123456)
    lookups = unspentOutputs utxos <>
      validator val
    valHash = validatorHash val

    constraints = mustPayToScript valHash unitDatum DatumWitness value
  log $ show valHash
  txId <- submitTxFromConstraints lookups constraints
  log $ show $ txId
  awaitTxConfirmed txId

unlock :: Contract Unit
unlock = do
  val <- getValidator
  mUtxos <- getWalletUtxos
  utxos <- liftMaybe (error "no utxos") mUtxos
  valUtxos <- utxosAt (scriptHashAddress (validatorHash val) Nothing)
  scriptRecord <- liftMaybe (error "can't find any script utxo") (findMax valUtxos)
  log $ show $ scriptRecord
  let
    lookups = unspentOutputs utxos
      <> unspentOutputs valUtxos
      <>
        validator val

    scriptInput = scriptRecord.key

    constraints = mustSpendScriptOutput scriptInput unitRedeemer
  txId <- submitTxFromConstraints lookups constraints
  log $ show $ txId
