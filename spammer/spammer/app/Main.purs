module Main (main) where

import Contract.Prelude

import Contract.Config (ContractParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Control.Monad.State (StateT, execStateT)
import Control.Safely (replicateM_)
import Effect.Aff (try)
import Spammer.Config (config)
import Spammer.Contracts.Lock (lock)
import Spammer.Contracts.Unlock (unlock)
import Spammer.State.Types (SpammerEnv, defaultSpammerEnv)
import Spammer.State.Update (countUtxos, loadAllLockedUtxos)

config' :: ContractParams 
config' = config "/tmp/wallet/wallet.skey" "localhost" "localhost" 1337 1442


main :: Effect Unit
main = do
  launchAff_ do
     trySpammer
     where
       spammer = runContract config' $ execStateT loop defaultSpammerEnv
       trySpammer = do 
          res <- try spammer
          either (\e -> (log $ show e) *> trySpammer) (\_ -> pure unit) res




loop ::  StateT SpammerEnv Contract Unit 
loop = do
    loadAllLockedUtxos
    loop1
    where
      loop1 = do 
        countUtxos
        replicateM_ 3 (lock *> unlock)
        loop1


