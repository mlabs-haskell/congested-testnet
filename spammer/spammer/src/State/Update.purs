module Spammer.State.Update where

import Contract.Prelude

import Contract.Transaction (TransactionInput)
import Contract.Value (lovelaceValueOf)
import Data.BigInt (fromInt)
import Data.Sequence as Seq
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..))

updateEnvValidator :: SpammerEnv -> Aff SpammerEnv
updateEnvValidator env = do
  validator <- getValidator
  pure <<< wrap $ (unwrap env) { validator = validator }

updateEnvValue :: SpammerEnv -> Aff SpammerEnv
updateEnvValue env = do
  let value = pure $ lovelaceValueOf (fromInt 1)
  pure <<< wrap $ (unwrap env) { value = value }

addUtxoForNextTransaction :: Boolean -> SpammerEnv -> SpammerEnv
addUtxoForNextTransaction x (SpammerEnv env) = wrap $ env { addUtxo = x }

updateEnvWallet :: SpammerEnv -> Aff SpammerEnv
updateEnvWallet env = do
  wallet <- getWallet'
  pure <<< wrap $ (unwrap env) { wallet = wallet }

updateTxInputsUsed :: forall f. Foldable f => f TransactionInput -> SpammerEnv -> SpammerEnv
updateTxInputsUsed inputs (SpammerEnv env) =
  let
    newInputs = Seq.fromFoldable inputs
    newSeq = Seq.take 2 $ env.txInputsUsed `Seq.append` newInputs
  in
    wrap $ env { txInputsUsed = newSeq }

