module Spammer.Query.Scripts where

import Contract.Prelude

import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy(..), Validator, mintingPolicyHash)
import Contract.Transaction (plutusV2Script)
import Contract.Value (getCurrencySymbol, mpsSymbol, singleton', Value)
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Data.BigInt as BInt
import Data.Maybe (Maybe)
import Spammer.Db (executeQuery)
import Spammer.Utils (liftJsonDecodeError)

type Result = Array { hex :: String }

-- WHERE time = (SELECT time FROM scripts ORDER BY time ASC LIMIT 1)
getValidator :: Aff (Maybe Validator)
getValidator = do
  let
    query' =
      """ 
            WITH cte AS (
                SELECT script 
                FROM scripts
                WHERE time = (SELECT MIN(time) FROM scripts)
                LIMIT 1
            )
            UPDATE scripts
            SET time = NOW()
            FROM cte
            WHERE scripts.script = cte.script
            RETURNING encode(scripts.script, 'hex') as hex;
        """

  json <- executeQuery query'
  result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
  pure do
    x <- head result
    bytes <- hexToByteArray x.hex
    pure <<< wrap <<< plutusV2Script $ bytes


getMintingPolicy :: Aff (Maybe MintingPolicy)
getMintingPolicy = pure do 
  let
    always_true_aiken_script_hex = "5251010000322253330034a229309b2b2b9a01"
  bytes <- hexToByteArray always_true_aiken_script_hex 
  pure <<< PlutusMintingPolicy <<< plutusV2Script $ bytes


-- getTokenName :: Aff (Maybe TokenName)
-- getTokenName = pure do
--   bytes <- byteArrayFromAscii "spamToken"
--   mkTokenName bytes


-- getCurrencySymbol' :: Maybe MintingPolicy -> Aff (Maybe CurrencySymbol)
-- getCurrencySymbol' mp = pure do
--   x <- mp
--   mpsSymbol <<< mintingPolicyHash $ x 



getValue' :: Maybe MintingPolicy -> Aff (Maybe Value) 
getValue' mpolicy  = pure do 
  policy <- mpolicy
  tokenBytes <- byteArrayFromAscii "spamToken"
  symbol <- mpsSymbol (mintingPolicyHash policy) 
  let symbolBytes = getCurrencySymbol symbol 
  singleton' symbolBytes tokenBytes (BInt.fromInt 1000)
      
  



