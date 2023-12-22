module Check where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.ScriptLookups (unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (TextEnvelopeType(..), decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (plutusV2Script, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletUtxos, withKeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.BigInt as BInt
import Data.Map (findMax)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Spammer.Query.Utils (decodeCborHexToBytes)
import Spammer.Query.Wallet (getWallet')

defaultTimeParams :: ContractTimeParams
defaultTimeParams =
  { syncWallet:
      -- As clarified in Eternl discord, they synchronize with the server every 2
      -- minutes, so 125 seconds would probably be enough.
      -- For other wallets, it is not very important
      { delay: Milliseconds 1_000.0, timeout: Seconds 125.0 }
  , syncBackend:
      -- Operations are costly, so the delay is 3 set to seconds
      { delay: Milliseconds 3_000.0, timeout: Seconds 120.0 }
  , awaitTxConfirmed:
      -- CIP-30 calls are cheap, so the delay can be just 1 second
      { delay: Milliseconds 1_000.0, timeout: Seconds infinity }
  , waitUntilSlot: { delay: Milliseconds 1_000.0 }
  }

defaultSynchronizationParams :: ContractSynchronizationParams
defaultSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false, beforeCip30Methods: true, beforeBalancing: true }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: true }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: true }
  }

config :: ContractParams
config =
  { backendParams: CtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig { host = "127.0.0.1" }
      , kupoConfig: defaultKupoServerConfig { path = Nothing, port = fromInt 1442 }
      }
      Nothing
  , networkId: TestnetId
  , logLevel: Info
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile "../../tmp/wallet0.skey") Nothing
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }

foreign import spamScript :: String

s :: String
s = "581a581801000032222533300453330044a229445280a4c26cacae69"

getValidator' :: Contract Validator
getValidator' =
  liftMaybe (error "Error decoding alwaysSucceeds") do
    let
      envelope = wrap { bytes: hexToByteArrayUnsafe s, description: "", type_: PlutusScriptV2 }
    Validator <$> plutusScriptV2FromEnvelope envelope

getValidator :: Contract Validator
getValidator = do
  envelope <- liftMaybe (error "Error decoding alwaysSucceeds") $ decodeTextEnvelope spamScript
  let
    bytes1 = byteArrayToHex $ (unwrap envelope).bytes
  log $ show spamScript
  log $ show bytes1
  liftMaybe (error "error here") $ Validator <$> (plutusScriptV2FromEnvelope envelope)

lock :: Validator -> Contract Unit
lock val = do
  mUtxos <- getWalletUtxos
  utxos <- liftMaybe (error "no utxos") mUtxos
  log $ show $ mUtxos
  log "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR"
  log $ show $ byteArrayToHex <<< fst <<< unwrap <<< unwrap $ val
  let
    value = lovelaceValueOf (BInt.fromInt 2123456)
    lookups = unspentOutputs utxos <>
      validator val

    constraints = mustPayToScript (validatorHash val) unitDatum DatumWitness value
  txId <- submitTxFromConstraints lookups constraints
  log $ show $ txId
  log "Successfully submitted"

unlock :: Validator -> Contract Unit
unlock val = do
  mUtxos <- getWalletUtxos
  utxos <- liftMaybe (error "no utxos") mUtxos
  valUtxos <- utxosAt (scriptHashAddress (validatorHash val) Nothing)
  scriptRecord <- liftMaybe (error "can't find any script utxo") (findMax valUtxos)
  log "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
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
  log "Successfully submitted"

-- decodeCborHexToBytes :: String -> Maybe ByteArray 
-- decodeCborHexToBytes cborHex = do
--   cborBa <- hexToByteArray cborHex
--   hush $ toByteArray $ wrap $ wrap cborBa
-- 510100003222253330044a229309b2b2b9a1
main :: Effect Unit
main = do
  log "hi"
  bytea <- liftMaybe (error "no cbor") $ decodeCborHexToBytes s
  let
    valid = wrap <<< plutusV2Script $ bytea
  launchAff_ do
    mwallet <- getWallet'
    wallet <- liftMaybe (error "no wallet") mwallet
    runContract config $ withKeyWallet wallet do
      -- valid <- getValidator
      -- lock valid
      unlock valid
-- lock
-- unlock
-- lock
-- unlock
-- lock
-- unlock

