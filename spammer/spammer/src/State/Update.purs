module Spammer.State.Update where

import Contract.Prelude

import Contract.Transaction (TransactionInput)
import Data.Sequence as Seq
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

