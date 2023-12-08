-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Main (main) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError, decodeJsonString, getField)
import Contract.Monad (launchAff_, liftContractAffM, runContract, throwContractError)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes, withKeyWallet)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (evalStateT, execStateT)
import Data.Argonaut (Json, decodeJson, jsonParser, parseJson, toObject)
import Data.Array as Data.Array
import Effect.Aff (error, try)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Spammer.Config (config)
import Spammer.Contracts (loopPayWalletToPubKey)
import Spammer.Contracts.Lock (getLockParams, lock)
import Spammer.Db (executeQuery)
import Spammer.Prometheus (getAvgMemPoolUsage)
import Spammer.Query.PubKeys (getPubKeyHash)
import Spammer.Query.Wallet (getWallet')
import Spammer.Start (startSpammer)
import Spammer.Types (SpammerEnv(..))

env = SpammerEnv {
  walletFrom : Nothing , 
  walletTo : Nothing ,  
  validatorTo : Nothing ,
  validatorFrom : Nothing, 
  valueToPay : Nothing,
  test : Just "heree"
  } 




main :: Effect Unit
main = do
  launchAff_ do
    runContract config do
       lockParams <- getLockParams env
       env' <- execStateT (lock lockParams) env
       log "HI"


-- main :: Effect Unit
-- main = do
--   launchAff_ do
--     runContract config do
--       keyWallet <- getWallet'
--       withKeyWallet keyWallet do
--         x <- liftContractAffM "NO" (pure <$> getAvgMemPoolUsage)
--         log $ show x
--         lock
--         unlock
--         pure unit
-- loopPayWalletToPubKey 


