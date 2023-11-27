-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Main (main) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError, decodeJsonString, getField)
import Contract.Monad (launchAff_, runContract)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes, withKeyWallet)
import Data.Argonaut (Json, decodeJson, jsonParser, parseJson, toObject)
import Data.Array as Data.Array
import Effect.Aff (error)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Spammer.Config (config)
import Spammer.Db (executeQuery)
import Spammer.Query.Wallet (getWallet')
import Spammer.Query.PubKeys (getPubKey)
import Spammer.Start (startSpammer)



main :: Effect Unit
main = do 
  -- startSpammer
  launchAff_ do
     keyWallet <- getWallet'
     runContract config $ withKeyWallet keyWallet do
        pubKeyToPay <- getPubKey 
        mutxos <- getWalletUtxos 
        pkeyhashes <- ownPaymentPubKeyHashes
        log $ show $ pkeyhashes 
        log $ show $ mutxos 
        log $ show $ pubKeyToPay 
     
     
     


  
       
  
