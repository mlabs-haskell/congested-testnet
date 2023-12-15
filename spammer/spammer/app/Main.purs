module Main (main) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError, decodeJsonString, getField)
import Contract.Monad (launchAff_, liftContractAffM, runContract, throwContractError)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes, withKeyWallet)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (StateT, evalStateT, execStateT, get, lift)
import Control.Safely (replicateM_)
import Data.Argonaut (Json, decodeJson, jsonParser, parseJson, toObject)
import Data.Array as Data.Array
import Data.Sequence (uncons)
import Effect.Aff (Fiber, error, joinFiber, launchAff, try)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Spammer.Config (config)
import Spammer.Contracts.Lock (lock)
import Spammer.Contracts.Unlock (unlock)
import Spammer.Db (executeQuery)
import Spammer.Prometheus (getAvgMemPoolUsage)
import Spammer.Query.PubKeys (getPubKeyHash)
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Lock (updateEnvForLock)
import Spammer.State.Types (SpammerEnv(..), defaultSpammerEnv)

config1 = config 1337 1442
config2 = config 1338 1443

main :: Effect Unit
main = do 
  launchAff_ do
    loop defaultSpammerEnv


loop :: SpammerEnv -> Aff SpammerEnv
loop env = do 
    env'  <- updateEnvForLock env 
    env'' <- runContract config1 do
      execStateT (replicateM_ 100 (unlock)) env'
    log $ show $ uncons ((unwrap env'').txInputsUsed)
    pure env''
    -- log "finish"
    -- loop env''






