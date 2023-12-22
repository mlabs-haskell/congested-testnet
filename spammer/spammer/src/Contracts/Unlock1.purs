module Spammer.Contracts.Unlock1 where

import Contract.Prelude

import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Monad (Contract)
import Contract.PlutusData (unitRedeemer)
import Contract.ScriptLookups (unspentOutputs)
import Contract.Scripts (Validator)
import Contract.Transaction (ScriptRef(..), TransactionInput, TransactionOutputWithRefScript(..), TransactionUnspentOutput(..), balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints (InputWithScriptRef(..), mustSpendScriptOutputUsingScriptRef)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (getUtxo)
import Contract.Wallet (KeyWallet, withKeyWallet)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.State (StateT, modify_)
import Control.Monad.State.Trans (get)
import Data.Map (singleton)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Set as Set
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.TxLocked (clearTxLocked, getTxLocked)
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (addUtxoForNextTransaction)

newtype UnLockParams = UnLockParams
  { wallet :: KeyWallet
  , validator :: Validator
  , txInputsUsed :: Seq TransactionInput
  , txLocked :: TransactionInput
  }

derive instance Newtype UnLockParams _
derive instance Generic UnLockParams _

extractUnLockPars :: SpammerEnv -> Contract (Maybe UnLockParams)
extractUnLockPars (SpammerEnv env) = do
  txLockedResult <- hush <$> try getTxLocked
  pure do
    wallet <- env.wallet
    { validator, txLocked } <- txLockedResult
    pure <<< UnLockParams $ { wallet, validator, txInputsUsed: env.txInputsUsed, txLocked }

unlock :: StateT SpammerEnv Contract Unit
unlock = do
  env <- get
  mpars <- lift $ extractUnLockPars env
  case mpars of
    Nothing -> pure unit
    Just (UnLockParams pars) -> do
      unlockResult <- lift $ try unlock'
      case unlockResult of
        Left e -> do
          lift $ log $ show e
          modify_ (addUtxoForNextTransaction true)
        Right (_ /\ _) -> do
          pure unit
          modify_ (addUtxoForNextTransaction false)
      -- modify_ (updateTxInputsUsed txInputs)
      where
      unlock' = withKeyWallet pars.wallet do
        mtxOutput <- getUtxo pars.txLocked
        txOutput <- liftMaybe (error "no utxos for locked utxo") mtxOutput
        let
          scriptRef = Just <<< PlutusScriptRef <<< unwrap $ pars.validator
          transactionOutputWithRefScript = TransactionOutputWithRefScript { output: txOutput, scriptRef }
          inputWithRefScript = SpendInput <<< TransactionUnspentOutput $ { input: pars.txLocked, output: transactionOutputWithRefScript }
          utxomap = singleton pars.txLocked transactionOutputWithRefScript
          lookups = unspentOutputs utxomap
          constraints = mustSpendScriptOutputUsingScriptRef pars.txLocked unitRedeemer inputWithRefScript
          balanceConstraints = mustNotSpendUtxosWithOutRefs (Set.fromFoldable pars.txInputsUsed)
        unbalancedTx <- mkUnbalancedTx lookups constraints
        balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
        signedBalancedTx <- signTransaction balancedTx
        txHash <- submit signedBalancedTx
        let
          txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
          txInputs = txBody.inputs
        clearTxLocked pars.txLocked
        log "unlock successfully"
        pure $ txInputs /\ txHash
