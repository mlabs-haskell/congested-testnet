module Spammer.Contracts.Unlock where

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
import Data.Map (fromFoldable, keys, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Sequence (Seq)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Aff (try)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.TxLocked (getTxLocked)
import Spammer.Query.Utils (decodeCborHexToBytes)
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (updateTxInputsUsed)

newtype UnLockParams = UnLockParams
  { wallet :: KeyWallet
  , validator :: Validator
  , txInputsUsed :: Seq TransactionInput
  }

derive instance Newtype UnLockParams _
derive instance Generic UnLockParams _

extractUnLockPars :: SpammerEnv -> Maybe UnLockParams
extractUnLockPars (SpammerEnv env) = do
  wallet <- env.wallet
  (validator /\ _) <- env.validator
  pure <<< UnLockParams $ { wallet, validator, txInputsUsed: env.txInputsUsed }

unlock :: StateT SpammerEnv Contract Unit
unlock = do
  env <- get
  let mpars = extractUnLockPars env
  case mpars of
    Nothing -> pure unit
    Just (UnLockParams pars) -> do
      result <- lift $ hush <$> try getTxLocked
      if isNothing result then
      txInputs <- lift unlock'
      -- modify_ (updateTxInputsUsed txInputs) 
      pure unit
        where
          unlock' = withKeyWallet pars.wallet do

             let
                validator' = if isNothing result then pars.validator else result.validator 
                lookups = validator validator
                valHash = validatorHash pars.validator
            --   constraints = mustPayToScript valHash unitDatum DatumWitness pars.value
            --   balanceConstraints = mustNotSpendUtxosWithOutRefs (Set.fromFoldable pars.txInputsUsed)
            --
            -- unbalancedTx <- mkUnbalancedTx lookups constraints
            -- balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
            -- signedBalancedTx <- signTransaction balancedTx
            -- txHash <- submit signedBalancedTx 
            -- log $ show unbalancedTx
            -- log $ show (unwrap $ unwrap balancedTx).body
            -- let
            --   txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
            --   txInputs = txBody.inputs
            -- log $ show txInputs
            --
            -- pure txInputs 
            pure unit
