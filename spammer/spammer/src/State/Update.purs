module Spammer.State.Update where

import Contract.Prelude

import Aeson (Aeson, decodeAeson)
import Contract.Prim.ByteArray (ByteArray(..))
import Contract.Transaction (TransactionInput(..))
import Contract.Value (lovelaceValueOf)
import Ctl.Internal.Serialization.Types (TransactionHash)
import Data.Argonaut (Json)
import Data.BigInt (fromInt)
import Data.Sequence (fromFoldable, append, length, take)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.Utxos (getUtxos')
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..), defaultSpammerEnv)
import Spammer.Query.Utils (liftJsonDecodeError)

updateEnvValidator :: SpammerEnv -> Aff SpammerEnv
updateEnvValidator env = do
  validator <- getValidator
  pure <<< wrap $ (unwrap env) { validator = validator }

updateEnvValue :: SpammerEnv -> Aff SpammerEnv
updateEnvValue env = do
  let value = pure $ lovelaceValueOf (fromInt 1_000_000)
  pure <<< wrap $ (unwrap env) { value = value }

updateEnvWallet :: SpammerEnv -> Aff SpammerEnv
updateEnvWallet env = do
  wallet <- getWallet'
  pure <<< wrap $ (unwrap env) { wallet = wallet }

updateTxInputsUsed :: forall f. Foldable f => f TransactionInput -> SpammerEnv -> SpammerEnv
updateTxInputsUsed inputs (SpammerEnv env) =
  let
    newInputs = fromFoldable inputs
    newSeq = take 100 $ env.txInputsUsed `append` newInputs
  in
    wrap $ env { txInputsUsed = newSeq }

