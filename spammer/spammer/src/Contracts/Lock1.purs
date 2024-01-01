module Spammer.Contracts.Lock1 (lock) where

import Contract.Prelude

import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (unitDatum)
import Contract.ScriptLookups (validator)
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (ScriptRef(..), TransactionInput, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints (DatumPresence(..), mustPayToPubKey, mustPayToScriptWithScriptRef)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Cont (lift)
import Control.Monad.State (StateT)
import Control.Monad.State.Trans (get, modify_)
import Data.Array as Array
import Data.BigInt as BInt
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Sequence as Seq
import Data.Set as Set
import Data.UInt as UInt
import Effect.Aff (try)
import Spammer.Query.Scripts (sampleValidator)
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (updateTxInputsUsed)

newtype LockParams = LockParams
  { 
   validator :: Maybe Validator
  , txInputsUsed :: Set.Set TransactionInput
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

extractLockPars :: SpammerEnv -> Contract LockParams
extractLockPars (SpammerEnv env) = do
  validator <- liftEffect sampleValidator 
  pure <<< LockParams $ { validator, txInputsUsed : Set.fromFoldable env.txInputsUsed }

lock :: StateT SpammerEnv Contract Unit
lock = do
  env <- get
  pars <- lift $ extractLockPars env
  lockResult <- lift $ try (lock' pars)
  case lockResult of
    Left e -> do
      lift $ log $ show e
    Right (txInputsUsed /\ mTxLocked ) -> do
      modify_ (updateTxInputsUsed txInputsUsed)


lock' :: LockParams -> Contract (Seq.Seq TransactionInput /\ Maybe TransactionInput )
lock' (LockParams pars) = do
  pkeyHash <- liftedM "no pubkeyHash" ownPaymentPubKeyHash  
  utxos <- liftedM "no utxos" $ getWalletUtxos
  log $ show (Map.size utxos)
  let
    lookups = maybe mempty validator pars.validator  
    constraints = 
          mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) <>
            mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) <>
            mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) <>
            maybe mempty (\val -> mustPayToScriptWithScriptRef (validatorHash val) unitDatum DatumWitness
        (PlutusScriptRef $ unwrap val) (lovelaceValueOf $ BInt.fromInt 1)) pars.validator 

    balanceConstraints = mustNotSpendUtxosWithOutRefs (pars.txInputsUsed)

  unbalancedTx <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
  signedBalancedTx <- signTransaction balancedTx
  txHash <- submit signedBalancedTx
  let
    txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
    txInputsUsed = Seq.fromFoldable txBody.inputs
    txOutputs = txBody.outputs
    mInd = Array.findIndex (isJust <<< _.scriptRef <<< unwrap) txOutputs
    mTxLocked = do 
     index <- mInd
     pure <<< wrap $ {index : UInt.fromInt index, transactionId : txHash} 


  log "================"
  log "locked successfully"

  pure $ txInputsUsed /\ mTxLocked 

