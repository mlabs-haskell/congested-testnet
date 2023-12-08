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
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, withKeyWallet)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.RWS (get)
import Control.Monad.State (StateT)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Serialization.Types (BigInt, TransactionHash)
import Data.Array (take)
import Data.BigInt as BInt
import Data.Map (findMax, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.Wallet (getWallet')
import Spammer.Types (SpammerEnv(..))
import Spammer.Utils (decodeCborHexToBytes)

-- s = "5850584e0100003222253330043253330055333005533300553330054a229445280a5114a0294452808008a50323300100148000894ccc018cdc42405000229444cc008008cdc0000a400429309b2b2b9a01"
--
-- getValidator :: Contract Validator
-- getValidator = do
--   arr <- liftMaybe (error "no parse") (decodeCborHexToBytes s)
--   pure $ wrap <<< plutusV2Script $ arr

newtype LockParams = LockParams {
  wallet :: KeyWallet , 
  validator :: Validator ,
  value  :: Value 
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

      -- value = lovelaceValueOf (BInt.fromInt 2123456)

lock :: LockParams -> StateT SpammerEnv Contract Unit 
lock (LockParams pars) = lift $ withKeyWallet pars.wallet do
  mUtxos <- getWalletUtxos
  let 
      utxos = do
        utxos <- mUtxos
        pure <<< fromFoldable <<< take 1 <<< toUnfoldable $ utxos 

  case utxos of  
      Nothing -> pure unit 
      Just utxos -> do 
        let
            lookups = unspentOutputs utxos <> validator pars.validator
            valHash = validatorHash pars.validator
            constraints = mustPayToScript valHash unitDatum DatumWitness pars.value
        log $ show utxos
        txId <- submitTxFromConstraints lookups constraints
        pure unit


getLockParams :: SpammerEnv -> Contract LockParams 
getLockParams (SpammerEnv env) = do 
  validator <- getValidator
  wallet <- getWallet'
  value <- pure $ lovelaceValueOf (BInt.fromInt 1000000)
  pure $ LockParams {validator, wallet, value} 




  





-- lock :: Contract Unit
-- lock = do
--   val <- getValidator
--   mUtxos <- getWalletUtxos
--   utxos <- liftMaybe (error "no utxos") mUtxos
--   let
--     value = lovelaceValueOf (BInt.fromInt 2123456)
--     lookups = unspentOutputs utxos <>
--       validator val
--     valHash = validatorHash val
--
--     constraints = mustPayToScript valHash unitDatum DatumWitness value
--   log $ show valHash
--   txId <- submitTxFromConstraints lookups constraints
--   log $ show $ txId
--   awaitTxConfirmed txId


-- lock' :: Maybe String -> MaybeT Contract Unit
-- lock' xx = do 
--   -- x <- pure xx 
--   pure xx
--   log $ "HI" 



-- lock' :: StateT SpammerEnv Contract Unit 
-- lock' = do
--   SpammerEnv env <- get
--   let args = do
--        wallet <- env.walletFrom
--        validator <- env.validatorTo
--        pure $ wallet /\ validator
--   case args of
--       Nothing -> pure unit
--       Just (wallet /\ validator) -> lift $ withKeyWallet wallet do 
--           mUtxos <- getWalletUtxos
--           let utxosToUse = do
--                 utxos <- mUtxos
--                 let 
--                     utxos2 = fromFoldable <<< take 1 <<< toUnfoldable $ utxos 
--                 pure utxos2
--           case utxosToUse of  
--               Nothing -> pure unit
--               Just utxos -> do 
--                  let
--                      lookups =  unspentOutputs utxos <> validator val
--                      constraints = mustPayToScript valHash unitDatum DatumWitness value





--
--             lookups =
--             valHash = validatorHash val
--
--             constraints = mustPayToScript valHash unitDatum DatumWitness value
--           log $ show valHash
--           txId <- submitTxFromConstraints lookups constraints
--           log $ show $ txId
--           awaitTxConfirmed txId





     

    
    


  






-- unlock :: Contract Unit
-- unlock = do
--   val <- getValidator
--   mUtxos <- getWalletUtxos
--   utxos <- liftMaybe (error "no utxos") mUtxos
--   valUtxos <- utxosAt (scriptHashAddress (validatorHash val) Nothing)
--   scriptRecord <- liftMaybe (error "can't find any script utxo") (findMax valUtxos)
--   log $ show $ scriptRecord
--   let
--     lookups = unspentOutputs utxos
--       <> unspentOutputs valUtxos
--       <>
--         validator val
--
--     scriptInput = scriptRecord.key
--
--     constraints = mustSpendScriptOutput scriptInput unitRedeemer
--   txId <- submitTxFromConstraints lookups constraints
--   log $ show $ txId
