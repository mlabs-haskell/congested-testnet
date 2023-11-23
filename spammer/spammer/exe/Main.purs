-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Main (main) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError, decodeJsonString, getField)
import Contract.Monad (launchAff_)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.QueryM.Ogmios (aesonObject)
import Data.Argonaut (Json, decodeJson, jsonParser, parseJson, toObject)
import Data.Array as Data.Array
import Effect.Aff (error)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Spammer.Db (executeQuery)

type KeyFile = { 
  type :: String,
  description :: String,
  cborHex :: String
  }



main :: Effect Unit
main = do 
  -- log "hi"
  utxo1_skey <- readTextFile UTF8 "/home/maxim/work/projects/congested-testnet/cardano-conf/utxo-keys/utxo1.skey"
  utxo1_pkey <- readTextFile UTF8 "/home/maxim/work/projects/congested-testnet/cardano-conf/utxo-keys/utxo1.vkey"
  let 
      json :: Either JsonDecodeError KeyFile
      json = parseJson utxo1_skey >>= decodeJson  
  key <- case json of
      Left e -> throw "decode error"
      Right key -> pure key

  log $ key.description
  launchAff_ do
     res <- executeQuery "select * from pkeys;"
     -- h <- liftMaybe (error "no array") (head res)
     log $ show $ Data.Array.null res 
     -- log "hi"
       
  
