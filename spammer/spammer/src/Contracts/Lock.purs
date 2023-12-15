module Spammer.Contracts.Lock where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionInput(..), awaitTxConfirmed, balanceTx, balanceTxWithConstraints, plutusV1Script, plutusV2Script, signTransaction, submit, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToPubKey, mustPayToScript, mustSpendPubKeyOutput, mustSpendScriptOutput)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, ownPaymentPubKeyHash, withKeyWallet)
import Contracts.Utils (getInputUtxos)
import Control.Alternative (guard)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.State (StateT)
import Control.Monad.State.Trans (modify, get, modify_)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head, take, length)
import Data.BigInt as BInt
import Data.Map (fromFoldable, keys, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.TxLocked (insertTxLocked)
import Spammer.Query.Utils (decodeCborHexToBytes)
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (updateTxInputsUsed)
import Data.BigInt as BInt

newtype LockParams = LockParams
  { wallet :: KeyWallet
  , validator :: Validator
  , valId :: String
  , value :: Value
  , txInputsUsed :: Seq TransactionInput
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

extractLockPars :: SpammerEnv -> Maybe LockParams
extractLockPars (SpammerEnv env) = do
  wallet <- env.wallet
  (validator /\ valId) <- env.validator
  value <- env.value
  pure <<< LockParams $ { wallet, valId, validator, value, txInputsUsed: env.txInputsUsed }

lock :: StateT SpammerEnv Contract Unit
lock = do
  env <- get
  let mpars = extractLockPars env
  case mpars of
    Nothing -> pure unit
    Just (LockParams pars) -> do
      lockResult <- lift $ try lock'
      case lockResult of
        Left _ -> do 
           lift $ log "lock failed"
           pure unit
        Right (txInputs /\ txHash ) -> do 
          modify_ (updateTxInputsUsed txInputs)
          lift $ insertTxLocked txHash "0" pars.valId
      where
      lock' = withKeyWallet pars.wallet do
        mownPkeyHash <- ownPaymentPubKeyHash 
        ownPkeyHash <- liftMaybe (error "no pubkeyHash") mownPkeyHash 
        let
          lookups = validator pars.validator
          valHash = validatorHash pars.validator
          constraints = mustPayToScript valHash unitDatum DatumWitness pars.value <>
                        mustPayToPubKey ownPkeyHash (lovelaceValueOf $ BInt.fromInt 2_000_000)

          balanceConstraints = mustNotSpendUtxosWithOutRefs (Set.fromFoldable pars.txInputsUsed)

        unbalancedTx <- mkUnbalancedTx lookups constraints
        balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
        signedBalancedTx <- signTransaction balancedTx
        txHash <- submit signedBalancedTx
        let
          txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
          txInputs = txBody.inputs
        -- log $ show $ pars.txInputsUsed 
        -- log "================"
        -- log $ show txInputs
        log "locked successfully"

        pure $ txInputs /\ txHash

