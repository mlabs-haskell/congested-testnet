module Spammer.Contracts.Lock1 where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, liftContractAffM, liftedM, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (ScriptRef(..), TransactionHash(..), TransactionInput(..), awaitTxConfirmed, balanceTx, balanceTxWithConstraints, plutusV1Script, plutusV2Script, signTransaction, submit, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToPubKey, mustPayToScript, mustPayToScriptWithScriptRef, mustSpendPubKeyOutput, mustSpendScriptOutput)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Coin(..), Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, ownPaymentPubKeyHash, ownPaymentPubKeyHashes, withKeyWallet)
import Contracts.Utils (getInputUtxos)
import Control.Alternative (guard)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.State (StateT)
import Control.Monad.State.Trans (modify, get, modify_)
import Ctl.Internal.Cardano.Types.Value (Coin(..), Value(..)) as Value
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head, length, take)
import Data.Array as Array
import Data.BigInt as BInt
import Data.Map (fromFoldable, keys, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator, getValidatorContract)
import Spammer.Query.TxLocked (insertTxLocked)
-- import Spammer.Query.TxRecentlyUsed (insertTxRecentlyUsed)
import Spammer.Query.Utils (decodeCborHexToBytes)
import Spammer.Query.Wallet (genNewPubKeyHash, getWallet')
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (addUtxoForNextTransaction, updateTxInputsUsed)

newtype LockParams = LockParams
  { wallet :: KeyWallet
  , validator :: Validator
  , value :: Value
  , valId :: String
  , txInputsUsed :: Seq TransactionInput
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

extractLockPars :: SpammerEnv -> Contract (Maybe LockParams)
extractLockPars (SpammerEnv env) = do
  mVal <- getValidatorContract
  pure do
    wallet <- env.wallet
    (validator /\ valId) <- mVal
    value <- env.value
    pure <<< LockParams $ { wallet, valId, validator, value, txInputsUsed: env.txInputsUsed }

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
          -- lift $ insertTxRecentlyUsed txInputs
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

          balanceConstraints = mustNotSpendUtxosWithOutRefs (Set.empty)

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

