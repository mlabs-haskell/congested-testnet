module Main (main) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError, decodeJsonString, getField)
import Contract.Monad (launchAff_, liftContractAffM, runContract, throwContractError)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes, withKeyWallet)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (StateT, evalStateT, execStateT, get, lift)
import Data.Argonaut (Json, decodeJson, jsonParser, parseJson, toObject)
import Data.Array as Data.Array
import Effect.Aff (error, joinFiber, launchAff, try)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Spammer.Config (config)
import Spammer.Contracts.Lock (lock)
import Spammer.Db (executeQuery)
import Spammer.Prometheus (getAvgMemPoolUsage)
import Spammer.Query.PubKeys (getPubKeyHash)
import Spammer.Query.Wallet (getWallet')
import Spammer.Start (startSpammer)
import Spammer.State.Lock (updateEnvForLock)
import Spammer.State.Types (SpammerEnv(..), defaultSpammerEnv)

main :: Effect Unit
main = do
  fiber_env <- launchAff do
    env <- updateEnvForLock defaultSpammerEnv
    runContract config do
      execStateT lock env
  launchAff_ do
    s <- joinFiber fiber_env
    runContract config do
      pure unit

