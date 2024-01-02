module Spammer.Contracts.Unlock (unlock) where

import Contract.Prelude

import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (unitRedeemer)
import Contract.ScriptLookups (unspentOutputs)
import Contract.Transaction (TransactionInput, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints (InputWithScriptRef(..), mustSpendScriptOutputUsingScriptRef)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (UtxoMap)
import Control.Monad.Cont (lift)
import Control.Monad.State (StateT, modify_)
import Control.Monad.State.Trans (get)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Set as Set
import Effect.Aff (try)
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (deleteHeadTxLocked, updateTxInputsUsed)


newtype UnLockParams = UnLockParams
  { 
   txInputsUsed :: Seq.Seq TransactionInput
  , txLocked :: UtxoMap 
  }

derive instance Newtype UnLockParams _
derive instance Generic UnLockParams _


utxoMapToInputs :: UtxoMap -> Maybe (TransactionInput /\ InputWithScriptRef)
utxoMapToInputs utxoMap = do
  rec <- Map.findMax utxoMap
  let
    transactionInput = rec.key  
    transactionOutputWithRefScript = rec.value
    transactionUnspentOutput = wrap {input : transactionInput, output : transactionOutputWithRefScript} 
    inputWithScriptRef = SpendInput transactionUnspentOutput 
  pure (transactionInput /\ inputWithScriptRef)
  


  

extractUnLockPars :: SpammerEnv -> Contract (Maybe UnLockParams)
extractUnLockPars (SpammerEnv env) = do
  let
    size = Seq.length env.txLocked
  log "============== number Tx locked on script ==============="
  log $ show size 
  pure $ if size < 30 then Nothing else do
    txLocked <- Seq.head env.txLocked
    pure <<< UnLockParams $ {txInputsUsed: env.txInputsUsed, txLocked }

unlock :: StateT SpammerEnv Contract Unit
unlock = do
  env <- get
  mpars <- lift $ extractUnLockPars env
  case mpars of
    Nothing -> pure unit
    Just pars -> do
      unlockResult <- lift $ try (unlock' pars)
      case unlockResult of
        Left e -> do
          lift $ log $ show e
        Right txInputs -> do
          modify_ (updateTxInputsUsed txInputs)
          modify_ deleteHeadTxLocked 

unlock' :: UnLockParams -> Contract (Set.Set TransactionInput) 
unlock' (UnLockParams {txLocked, txInputsUsed}) = do
  let
      mInputs = utxoMapToInputs txLocked 
  (transactionInput /\ inputWithScriptRef) <- liftedM "no inputs to unlock" (pure mInputs)
  let
    lookups = unspentOutputs txLocked 
    constraints = mustSpendScriptOutputUsingScriptRef transactionInput unitRedeemer inputWithScriptRef
    balanceConstraints = mustNotSpendUtxosWithOutRefs (Set.fromFoldable txInputsUsed)
  unbalancedTx <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
  signedBalancedTx <- signTransaction balancedTx
  _ <- submit signedBalancedTx
  let
    txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
    txInputs = txBody.inputs
  log "unlock successfully"
  pure $  txInputs 
