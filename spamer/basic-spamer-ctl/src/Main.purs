module Spamer.Main where


import Contract.Prelude
import Contract.Address (NetworkId(..), ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Config (ContractParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (PlutusScript(..), Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (Language(..), submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.BigInt as BInt
import Data.Map (findMax)
import Data.Maybe (Maybe(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- getValidator :: Effect Validator 
-- getValidator = do
--   str <-  "../../validator.uplc"
--   bytes <- liftMaybe (error "can't convert script to bytearray") (byteArrayFromAscii str)
--   pure $ Validator $ PlutusScript (bytes /\ PlutusV2)

foreign import spamScript :: String

getValidator :: Contract Validator
getValidator =
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope spamScript 
    Validator <$> plutusScriptV1FromEnvelope envelope


config :: ContractParams 
config =
      { backendParams: CtlBackendParams 
      { ogmiosConfig: defaultOgmiosWsConfig {host = "127.0.0.1"} 
        , kupoConfig: defaultKupoServerConfig {path = Nothing, port = fromInt 1442}
        } Nothing
      , networkId: TestnetId 
      , logLevel: Trace
      , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile "../../cardano-conf/wallet0.skey") Nothing  
      , customLogger: Nothing
      , suppressLogs : false 
      -- , suppressLogs : true 
      , hooks : emptyHooks
      }


lock :: Effect Unit
lock = do 
  launchAff_ do
    runContract config do
      val <- getValidator 
      mOwnPkeyHash <- ownPaymentPubKeyHash 
      pKhash <- liftMaybe (error "no public key hash") mOwnPkeyHash 
      mUtxos <- getWalletUtxos
      utxos <- liftMaybe (error "no utxos") mUtxos
      let
          value = lovelaceValueOf (BInt.fromInt 2123456) 
          lookups :: ScriptLookups Void 
          lookups = unspentOutputs utxos <>
                    validator val

          constraints :: TxConstraints Void Void 
          constraints = mustPayToScript (validatorHash val) unitDatum DatumWitness value 
      -- log $ show $ utxos 
      txId <- submitTxFromConstraints lookups constraints
      log $ show $ txId 
      log "Successfully submitted"

unlock :: Effect Unit
unlock = do 
  launchAff_ do
    runContract config do
      val <- getValidator 
      mOwnPkeyHash <- ownPaymentPubKeyHash 
      pKhash <- liftMaybe (error "no public key hash") mOwnPkeyHash 
      mUtxos <- getWalletUtxos
      utxos <- liftMaybe (error "no utxos") mUtxos
      valUtxos <- utxosAt (scriptHashAddress (validatorHash val) Nothing)  
      scriptRecord <- liftMaybe (error "can't find any script utxo") (findMax valUtxos) 
      log $ show $ scriptRecord 
      let
          lookups :: ScriptLookups Void 
          lookups = unspentOutputs utxos <>
                    unspentOutputs valUtxos <>
                    validator val


          scriptInput = scriptRecord.key

          constraints :: TxConstraints Void Void 
          constraints = mustSpendScriptOutput scriptInput unitRedeemer 
      txId <- submitTxFromConstraints lookups constraints
      log $ show $ txId 
      log "Successfully submitted"
      -- waitNSlots (fromInt' 1)

    

main :: Effect Unit
main = do
  lock
  -- unlock
  







