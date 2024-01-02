module Spammer.Contracts.Lock  where

import Contract.Prelude

import Contract.BalanceTxConstraints (mustNotSpendUtxosWithOutRefs)
import Contract.Credential (Credential(..))
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (unitDatum)
import Contract.ScriptLookups (validator)
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (ScriptRef(..), TransactionHash, TransactionInput, TxBody(..), balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints (DatumPresence(..), mustPayToPubKey, mustPayToScriptWithScriptRef)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (UtxoMap)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Cont (lift)
import Control.Monad.State (StateT)
import Control.Monad.State.Trans (get, modify_)
import Ctl.Internal.Cardano.Types.Value as Cardano.Value
import Ctl.Internal.Plutus.Types.Value as Plutus.Value
import Data.Array as Array
import Data.BigInt as BInt
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Sequence as Seq
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.UInt as UInt
import Effect.Aff (try)
import Effect.Exception (message)
import Spammer.Contracts.GetFunds (getFundsFromFaucet)
import Spammer.Query.Scripts (sampleValidator)
import Spammer.State.Types (SpammerEnv(..))
import Spammer.State.Update (updateTxInputsUsed, updateTxLocked)

newtype LockParams = LockParams
  { 
   validator :: Maybe Validator
  , txInputsUsed :: Set.Set TransactionInput
  , numberUtxos :: Int 
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

extractLockPars :: SpammerEnv -> Contract LockParams
extractLockPars (SpammerEnv env) = do
  validator <- liftEffect sampleValidator 
  pure <<< LockParams $ { validator, txInputsUsed : Set.fromFoldable env.txInputsUsed, numberUtxos : env.numberUtxos }


lock :: StateT SpammerEnv Contract Unit
lock = do
  env <- get
  pars <- lift $ extractLockPars env
  lockResult <- lift $ try (lock' pars)
  case lockResult of
    Left e -> do
      let
        message' = message e
      if contains (Pattern "Insufficient balance") message' 
      then do
         pkh <- lift $ liftedM "no own pKeyHash" ownPaymentPubKeyHash
         log "request faucet" 
         lift <<< liftEffect $ getFundsFromFaucet pkh
      else
         log $  message e
    Right (txInputsUsed /\ mTxLocked ) -> do
      modify_ (updateTxInputsUsed txInputsUsed)
      maybe (pure unit) (\txLocked -> modify_ (updateTxLocked txLocked)) mTxLocked



lock' :: LockParams -> Contract (Seq.Seq TransactionInput /\ Maybe UtxoMap)
lock' (LockParams pars) = do
  pkeyHash <- liftedM "no pubkeyHash" ownPaymentPubKeyHash  
  let
    lookups = maybe mempty validator pars.validator  
    addutxos = mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) <>
               mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) <>
               mustPayToPubKey pkeyHash (lovelaceValueOf $ BInt.fromInt 1_000_000) 
    lockOnScript = maybe mempty (\val -> mustPayToScriptWithScriptRef (validatorHash val) unitDatum DatumWitness
        (PlutusScriptRef $ unwrap val) (lovelaceValueOf $ BInt.fromInt 1)) pars.validator 
    constraints = (if pars.numberUtxos < 10 then addutxos else mempty) <> lockOnScript

    balanceConstraints = mustNotSpendUtxosWithOutRefs (pars.txInputsUsed)

  if constraints == mempty then log "====empty lock====" else pure unit

  unbalancedTx <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTxWithConstraints unbalancedTx balanceConstraints
  signedBalancedTx <- signTransaction balancedTx
  txHash <- submit signedBalancedTx
  let
    txBody = unwrap <<< _.body <<< unwrap <<< unwrap $ signedBalancedTx
    txInputsUsed = Seq.fromFoldable txBody.inputs
    mTxLocked = getLockedTx txHash (wrap txBody) 

  -- log "================"
  log "locked successfully"
  -- log $ show mTxLocked

  pure $ txInputsUsed /\ mTxLocked  


getLockedTx :: TransactionHash -> TxBody -> Maybe UtxoMap
getLockedTx txHash (TxBody txBody) = do  
  let
    txOutputs = txBody.outputs
    mIndexWithScript = Array.findIndex (isJust <<< _.scriptRef <<< unwrap) txOutputs
  index <- mIndexWithScript
  transactionOutput <- Array.index txOutputs index
  scriptRef' <- (unwrap transactionOutput).scriptRef
  plutusScript <- case scriptRef' of
      (PlutusScriptRef plutusScript) -> Just plutusScript  
      _ -> Nothing 
  let
    val = wrap plutusScript
    valHash = validatorHash val
    scriptHash = unwrap valHash 
    addressCredential = ScriptCredential valHash
    address = wrap { addressCredential, addressStakingCredential : Nothing}  

    (Cardano.Value.Value coin _ ) = (unwrap transactionOutput).amount 
    bigInt = Cardano.Value.getLovelace coin
    amount = Plutus.Value.lovelaceValueOf bigInt 
    datum = (unwrap transactionOutput).datum
    referenceScript = Just scriptHash  
    output = wrap {address, amount, datum, referenceScript} 
    scriptRef = Just <<< PlutusScriptRef $ plutusScript
    transactionInput :: TransactionInput
    transactionInput = wrap {index : (UInt.fromInt index), transactionId : txHash}
    transactionOutputWithRefScript = wrap $ {output, scriptRef} 
  pure $ Map.singleton transactionInput transactionOutputWithRefScript





