module Spammer.Start (startSpammer) where
  
import Contract.Prelude

import Aeson (JsonDecodeError)
import Contract.Monad (launchAff_)
import Data.Argonaut (class DecodeJson, decodeJson, parseJson, printJsonDecodeError)
import Data.String (drop)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Spammer.Db (executeQuery)
import Spammer.Types (KeyFile, CborHex)



startSpammer :: Effect Unit 
startSpammer = do
  loadGenesisUtxoKeysDb

loadGenesisUtxoKeysDb :: Effect Unit
loadGenesisUtxoKeysDb = do 
  let 
      dir = "/home/maxim/work/projects/congested-testnet/cardano-conf/utxo-keys/"
  utxo1_skey_file<- readTextFile UTF8 (dir <> "utxo1.skey")
  utxo1_pkey_file <- readTextFile UTF8 (dir <> "utxo1.vkey")
  utxo2_skey_file <- readTextFile UTF8 (dir <> "utxo2.skey")
  utxo2_pkey_file <- readTextFile UTF8 (dir <> "utxo2.vkey")
  utxo1_skey_hex <- liftJsonDecodeError (parseKeyFromJsonString utxo1_skey_file) 
  utxo1_pkey_hex <- liftJsonDecodeError (parseKeyFromJsonString utxo1_pkey_file) 
  utxo2_skey_hex <- liftJsonDecodeError (parseKeyFromJsonString utxo2_skey_file) 
  utxo2_pkey_hex <- liftJsonDecodeError (parseKeyFromJsonString utxo2_pkey_file) 
  let 
      query = "INSERT INTO pkeys (pkey, pubkey) VALUES " <>  
                "('" <> utxo1_skey_hex <> "', '" <> utxo1_pkey_hex <> "'), " <> 
                "('" <> utxo2_skey_hex <> "', '" <> utxo2_pkey_hex <> "') " <>
                "ON CONFLICT (pkey) DO NOTHING;"
  launchAff_ do
     executeQuery query

       
  




parseKeyFromJsonString :: String -> Either JsonDecodeError CborHex 
parseKeyFromJsonString jsonString = do
      keyFile :: KeyFile <- parseJson jsonString >>= decodeJson  
      let
          cborHex = keyFile.cborHex 
          cborHexWithoutHeader = drop 4 cborHex
      pure cborHexWithoutHeader


liftJsonDecodeError :: forall a. DecodeJson a => Either JsonDecodeError a -> Effect a 
liftJsonDecodeError eitherErrA = do 
  case eitherErrA of
      Left e -> throw $ printJsonDecodeError e 
      Right x -> pure x 






  
