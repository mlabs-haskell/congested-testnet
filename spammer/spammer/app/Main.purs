module Main (main) where

import Contract.Prelude

import Contract.Config (ContractParams)
import Contract.Monad (launchAff_, runContract)
import Control.Monad.State (execStateT)
import Control.Safely (replicateM_)
import Data.Sequence (uncons)
import Spammer.Config (config)
import Spammer.Contracts.Lock1 (lock)
import Spammer.Query.Scripts (_sampleValidator, sampleValidator)
import Spammer.State.Lock (updateEnvForLock)
import Spammer.State.Types (SpammerEnv, defaultSpammerEnv)

config' :: ContractParams 
config' = config "/tmp/wallet/wallet.skey" "localhost" "localhost" 1337 1442

main :: Effect Unit
main = do
  val <- sampleValidator
  log $ show val

-- main :: Effect Unit
-- main = do
--   launchAff_ do
--     loop defaultSpammerEnv
--
-- loop :: SpammerEnv -> Aff SpammerEnv
-- loop env = do
--   env' <- updateEnvForLock env
--   env'' <- runContract config' do
--     execStateT (replicateM_ 1 (lock)) env'
--   log $ show $ uncons ((unwrap env'').txInputsUsed)
--   pure env''
--
