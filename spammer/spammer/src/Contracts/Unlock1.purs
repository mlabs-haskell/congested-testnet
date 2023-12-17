module Spammer.Contracts.Unlock1 where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (ScriptRef(..), TransactionInput(..), TransactionOutput(..), TransactionOutputWithRefScript(..), TransactionUnspentOutput(..), awaitTxConfirmed, balanceTx, balanceTxWithConstraints, plutusV1Script, plutusV2Script, signTransaction, submit, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), InputWithScriptRef(..), TxConstraints, mustPayToPubKey, mustPayToScript, mustSpendPubKeyOutput, mustSpendScriptOutput, mustSpendScriptOutputUsingScriptRef)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (UtxoMap, getUtxo, utxosAt)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, withKeyWallet)
import Contracts.Utils (getInputUtxos)
import Control.Alternative (guard)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.State (StateT, modify_)
import Control.Monad.State.Trans (modify, get)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head, take, length)
import Data.BigInt as BInt
import Data.List (List)
import Data.Map (empty, fromFoldable, keys, singleton, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Sequence (Seq)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.TxLocked (clearTxLocked, getTxLocked)
import Spammer.Query.Utils (decodeCborHexToBytes)
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (updateTxInputsUsed)

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
    {validator , txLocked}  <- txLockedResult 
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
           pure unit
        Right (txInputs /\ _) -> pure unit
          -- modify_ (updateTxInputsUsed txInputs)
      where
      unlock' = withKeyWallet pars.wallet do
        mtxOutput <- getUtxo pars.txLocked 
        txOutput <- liftMaybe (error "no utxos for locked utxo") mtxOutput 
        let
          scriptRef = Just <<< PlutusScriptRef <<< unwrap $ pars.validator 
          transactionOutputWithRefScript = TransactionOutputWithRefScript {output : txOutput, scriptRef}
          inputWithRefScript = SpendInput <<< TransactionUnspentOutput $ {input : pars.txLocked, output : transactionOutputWithRefScript}
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
