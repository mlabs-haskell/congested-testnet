module Spammer.State.Lock where

import Contract.Prelude

import Aeson (Aeson, decodeAeson)
import Contract.Prim.ByteArray (ByteArray(..))
import Contract.Value (lovelaceValueOf)
import Data.Argonaut (Json)
import Data.BigInt (fromInt)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.Utxos (getUtxos')
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..), defaultSpammerEnv)
import Spammer.Utils (liftJsonDecodeError)

updateEnvForLock :: SpammerEnv -> Aff SpammerEnv
updateEnvForLock (SpammerEnv env) = do
  validator <- getValidator
  wallet <- getWallet'
  utxos <- getUtxos'
  let value = pure $ lovelaceValueOf (fromInt 1_000_000)
  pure <<< wrap $ (unwrap defaultSpammerEnv) { validator = validator, wallet = wallet, value = value, utxos = utxos }

