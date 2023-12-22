module Spammer.State.Lock where

import Contract.Prelude

import Spammer.State.Types (SpammerEnv)
import Spammer.State.Update (updateEnvValidator, updateEnvValue, updateEnvWallet)

updateEnvForLock :: SpammerEnv -> Aff SpammerEnv
updateEnvForLock = updateEnvWallet >=> updateEnvValue >=> updateEnvValidator

