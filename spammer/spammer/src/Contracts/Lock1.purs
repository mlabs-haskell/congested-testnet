module Spammer.Contracts.Lock1 where

import Contract.Prelude

import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (unitDatum)
import Contract.ScriptLookups (validator)
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (ScriptRef(..), TransactionInput, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints (DatumPresence(..), mustPayToPubKey, mustPayToScriptWithScriptRef)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, ownPaymentPubKeyHashes, withKeyWallet)
import Control.Monad.Cont (lift)
import Control.Monad.State (StateT)
import Control.Monad.State.Trans (get, modify_)
import Data.Array (head)
import Data.Array as Array
import Data.BigInt as BInt
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (try)
import Spammer.Query.Scripts (getValidatorContract)
import Spammer.Query.TxLocked (insertTxLocked)
import Spammer.Query.TxRecentlyUsed (getTxRecentlyUsed, insertTxRecentlyUsed)
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (addUtxoForNextTransaction)

newtype LockParams = LockParams
  { wallet :: KeyWallet
  , validator :: Validator
  , value :: Value
  , valId :: String
  , txInputsUsed :: Set.Set TransactionInput
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

extractLockPars :: SpammerEnv -> Contract (Maybe LockParams)
extractLockPars (SpammerEnv env) = do
  mVal <- getValidatorContract
  txInputsUsed <- getTxRecentlyUsed 
  pure do
    wallet <- env.wallet
    (validator /\ valId) <- mVal
    value <- env.value
    pure <<< LockParams $ { wallet, valId, validator, value, txInputsUsed }

lock :: StateT SpammerEnv Contract Unit
lock = do
  env <- get
  mpars <- lift $ extractLockPars env
  case mpars of
    Nothing -> pure unit
    Just (LockParams pars) -> do
      lockResult <- lift $ try lock'
      case lockResult of
        Left e -> do
          lift $ log $ show e
          modify_ (addUtxoForNextTransaction true)
        Right (txInputs /\ txHash /\ ind) -> do
          modify_ (addUtxoForNextTransaction false)
          lift $ insertTxRecentlyUsed txInputs
          lift $ insertTxLocked txHash ind pars.valId
      where
      lock' = withKeyWallet pars.wallet do
        pkeyHash <- liftedM "no pubkeyHash" $ head <$> ownPaymentPubKeyHashes
        utxos <- liftedM "no utxos" $ getWalletUtxos
        log $ show (Map.size utxos)

        let
          lookups = validator pars.validator
          valHash = validatorHash pars.validator

          constraints =
            mustPayToScriptWithScriptRef valHash unitDatum DatumWitness
              (PlutusScriptRef $ unwrap pars.validator)
              pars.value <>
              if (unwrap env).addUtxo then
                mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) <>
                  mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000)
              else mempty

          balanceConstraints = mustNotSpendUtxosWithOutRefs (pars.txInputsUsed)

        unbalancedTx <- mkUnbalancedTx lookups constraints
        balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
        signedBalancedTx <- signTransaction balancedTx
        txHash <- submit signedBalancedTx
        let
          txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
          txInputs = txBody.inputs
          txOutputs = txBody.outputs
          ind = fromMaybe 0 $ Array.findIndex (isJust <<< _.scriptRef <<< unwrap) txOutputs

        log "================"
        log "locked successfully"

        pure $ txInputs /\ txHash /\ show ind

