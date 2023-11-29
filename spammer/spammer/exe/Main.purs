-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Main (main) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError, decodeJsonString, getField)
import Contract.Monad (launchAff_, runContract, throwContractError)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes, withKeyWallet)
import Data.Argonaut (Json, decodeJson, jsonParser, parseJson, toObject)
import Data.Array as Data.Array
import Effect.Aff (error, try)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Spammer.Config (config)
import Spammer.Contracts (loopPayWalletToPubKey)
import Spammer.Db (executeQuery)
import Spammer.Query.PubKeys (getPubKeyHash)
import Spammer.Query.Wallet (getWallet')
import Spammer.Start (startSpammer)



main :: Effect Unit
main = do 
  launchAff_ do
     runContract config do
        loopPayWalletToPubKey 

     
     
     


  
       
  
