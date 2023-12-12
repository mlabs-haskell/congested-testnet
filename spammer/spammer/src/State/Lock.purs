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
import Spammer.State.Update (updateEnvValidator, updateEnvValue, updateEnvWallet)

updateEnvForLock :: SpammerEnv -> Aff SpammerEnv
updateEnvForLock = updateEnvWallet >=> updateEnvValue >=> updateEnvValidator

