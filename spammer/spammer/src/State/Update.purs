module Spammer.State.Update where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract, liftedM)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput)
import Contract.Utxos (UtxoMap, utxosAt)
import Control.Monad.Cont (lift)
import Control.Monad.State (StateT, modify_)
import Ctl.Internal.Scripts (validatorHash)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Spammer.Query.Scripts (allValidators)
import Spammer.State.Types (SpammerEnv(..))


addUtxoForNextTransaction :: Boolean -> SpammerEnv -> SpammerEnv
addUtxoForNextTransaction x (SpammerEnv env) = wrap $ env { addUtxo = x }



updateTxInputsUsed :: forall f. Foldable f => f TransactionInput -> SpammerEnv -> SpammerEnv
updateTxInputsUsed inputs (SpammerEnv env) =
  let
    newInputs = Seq.fromFoldable inputs
    newSeq = Seq.take 100 $ env.txInputsUsed `Seq.append` newInputs
  in
    wrap $ env { txInputsUsed = newSeq }

updateTxLocked::  UtxoMap -> SpammerEnv -> SpammerEnv
updateTxLocked input (SpammerEnv env) =
  let
    newInputs = Seq.singleton input
    newSeq =  env.txLocked `Seq.append` newInputs
  in
    wrap $ env { txLocked = newSeq }

getUtxoFromValidator :: Validator -> Contract UtxoMap
getUtxoFromValidator val = do
  let 
      valHash = validatorHash val 
      address = scriptHashAddress valHash Nothing 
  utxosAt address


loadAllLockedUtxos :: StateT SpammerEnv Contract Unit 
loadAllLockedUtxos = do
  lockedInputs <- lift do
    vals <- liftedM "no validators"  (liftEffect allValidators) 
    allLockedUtxos :: UtxoMap <- foldM (\allUtxos val -> (Map.union allUtxos ) <$> (getUtxoFromValidator val)) Map.empty vals

    let 
        seqTuple = Map.toUnfoldable allLockedUtxos
        seqOfUtxos :: Seq.Seq UtxoMap
        seqOfUtxos =  (\(k /\ v) -> Map.singleton k v) <$> seqTuple  
    pure $ seqOfUtxos 
  log $ show $ Seq.length lockedInputs
  modify_ (\(SpammerEnv x) -> wrap (x {txLocked = x.txLocked <> lockedInputs }))
  




