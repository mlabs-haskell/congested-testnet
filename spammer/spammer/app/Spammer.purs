module Spammer where

import Contract.Prelude

import Contract.Monad (Contract, ContractParams, launchAff_, liftedM, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (validator)
import Contract.Scripts (validatorHash)
import Contract.Transaction (ScriptRef(..), TransactionInput, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), InputWithScriptRef(..), mustPayToPubKey, mustPayToScriptWithScriptRef, mustSpendScriptOutputUsingScriptRef)
import Contract.Utxos (UtxoMap)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletBalance, getWalletUtxos, ownPaymentPubKeyHash)
import Control.Safely (replicateM_)
import Data.Array as Array
import Data.BigInt as BInt
import Data.Map as Map
import Effect.Aff (try)
import Spammer.Config (config)
import Spammer.Query.Scripts (allValidators, sampleValidator)
import Spammer.State.Update (getUtxoFromValidator)

config' :: ContractParams 
config' = config "/wallet/wallet.skey" "ogmios.local" "kupo.local" 1337 1442


main :: Effect Unit
main = do 
  launchAff_ do
    runContract config' do 
       utxos <- liftedM "no utxos" getWalletUtxos
       balance <- liftedM "wallet balance" getWalletBalance
       log "total utxos"
       let 
           nutxos = Map.size utxos
       log $ show $ nutxos 
       if nutxos < 100 then generateUtxos else pure unit
       log "total balance"
       log $ show balance 
       replicateM_ 10 do
         replicateM_ 20 lock
         inputs <- utxoMapToInputs <$> getAllLockedUtxos
         log "total locked Tx"
         log $ show $ Array.length inputs
         traverse_ unlock inputs

generateUtxos :: Contract Unit
generateUtxos = do 
  pkh <- liftedM "no pkh" ownPaymentPubKeyHash 
  let
    paySelf = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 1_000_000)
    paySelfMany = mconcat $ Array.replicate 100 paySelf
  res <- submitTxFromConstraints mempty paySelfMany 
  log $ show res
  pure unit




lock ::  Contract Unit
lock = do
  mval <-  liftEffect sampleValidator  
  pkh <- liftedM "no pkh" ownPaymentPubKeyHash 
  let
    lookups = maybe mempty validator mval 
    paySelf = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 2_000_000)
    
    payScript val =  mustPayToScriptWithScriptRef 
          (validatorHash val) unitDatum DatumWitness 
          (PlutusScriptRef $ unwrap val)
          (lovelaceValueOf $ BInt.fromInt 1)
    constraints = maybe paySelf payScript mval

  res <- try $ submitTxFromConstraints lookups constraints
  log $ show $ res
  log $ maybe "paySelf" (const "payScript") mval




unlock :: (TransactionInput /\ InputWithScriptRef) -> Contract Unit
unlock (transactionInput /\ inputWithScriptRef) = do
  let
    constraints = mustSpendScriptOutputUsingScriptRef transactionInput unitRedeemer inputWithScriptRef
  _ <- try $ submitTxFromConstraints mempty constraints
  log "unlock"




getAllLockedUtxos :: Contract UtxoMap 
getAllLockedUtxos = do
    vals <- liftedM "no validators" (liftEffect allValidators)
    allLockedUtxos <-  foldM (\allUtxos val -> (Map.union allUtxos) <$> (getUtxoFromValidator val)) Map.empty vals
    log $ show $ Map.size allLockedUtxos
    pure allLockedUtxos 



utxoMapToInputs :: UtxoMap -> Array (TransactionInput /\ InputWithScriptRef)
utxoMapToInputs utxoMap = 
  let
    arr = Map.toUnfoldable utxoMap
    tupleToInputs (transactionInput /\ transactionOutputWithRefScript)  = 
      let
        transactionUnspentOutput = wrap { input: transactionInput, output: transactionOutputWithRefScript }
        inputWithScriptRef = SpendInput transactionUnspentOutput
      in 
      (transactionInput /\ inputWithScriptRef)
  in
    tupleToInputs <$> arr
    

