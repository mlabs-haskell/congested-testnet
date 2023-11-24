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
import Spammer.Types (KeyFile)
import Spammer.Start (startSpammer)



main :: Effect Unit
main = do 
  startSpammer
  
       
  
