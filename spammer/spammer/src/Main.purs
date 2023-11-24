module Spammer.Main where


import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (submitTxFromConstraints)
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




-- getValidator :: Contract Validator
-- getValidator =
--   liftMaybe (error "Error decoding alwaysSucceeds") do
--     envelope <- decodeTextEnvelope spamScript 
--     Validator <$> plutusScriptV2FromEnvelope envelope

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
      { delay: Milliseconds 1_000.0, timeout: Seconds infinity}
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
      { ogmiosConfig: defaultOgmiosWsConfig {host = "127.0.0.1"} 
        , kupoConfig: defaultKupoServerConfig {path = Nothing, port = fromInt 1442}
        } Nothing
      , networkId: TestnetId 
      , logLevel: Debug 
      , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile "../../tmp/wallet0.skey") Nothing  
      , customLogger: Nothing
      , suppressLogs : false 
      , hooks : emptyHooks
      , timeParams: defaultTimeParams 
      , synchronizationParams: defaultSynchronizationParams
      }

-- lock :: Contract Unit 
-- lock = do
--   val <- getValidator 
--   mUtxos <- getWalletUtxos 
--   utxos <- liftMaybe (error "no utxos") mUtxos
--   log $ show $ mUtxos 
--   let
--       value = lovelaceValueOf (BInt.fromInt 2123456) 
--       lookups = unspentOutputs utxos <>
--                 validator val
--
--       constraints = mustPayToScript (validatorHash val) unitDatum DatumWitness value 
--   txId <- submitTxFromConstraints lookups constraints
--   log $ show $ txId 
--   log "Successfully submitted"



-- unlock :: Contract Unit
-- unlock = do 
--   val <- getValidator 
--   mUtxos <- getWalletUtxos
--   utxos <- liftMaybe (error "no utxos") mUtxos
--   valUtxos <- utxosAt (scriptHashAddress (validatorHash val) Nothing)  
--   scriptRecord <- liftMaybe (error "can't find any script utxo") (findMax valUtxos) 
--   log $ show $ scriptRecord 
--   let
--       lookups = unspentOutputs utxos <>
--                 unspentOutputs valUtxos <>
--                 validator val
--
--
--       scriptInput = scriptRecord.key
--
--       constraints = mustSpendScriptOutput scriptInput unitRedeemer 
--   txId <- submitTxFromConstraints lookups constraints
--   log $ show $ txId 
--   log "Successfully submitted"

    
-- decodeCborHexToBytes :: String -> Maybe ByteArray 
-- decodeCborHexToBytes cborHex = do
--   cborBa <- hexToByteArray cborHex
--   hush $ toByteArray $ wrap $ wrap cborBa

-- s1 :: String
-- s1 = "4e4d01000033222220051200120011"
-- s2 :: String
-- s2 = "480100002221200101"
main :: Effect Unit
main = do 
  log "hi"
  -- arr <- liftMaybe (error "no parse") (decodeCborHexToBytes s2)
  -- let script = plutusV2Script arr
  -- log $ show $ script
  -- log $ show $ arr
  -- launchAff_ do
  --   runContract config do 
  --      lock
       -- unlock
       -- lock
       -- unlock
       -- lock
       -- unlock
       -- lock
       -- unlock

      







