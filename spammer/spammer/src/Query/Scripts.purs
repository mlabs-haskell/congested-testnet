module Spammer.Query.Scripts where

import Contract.Prelude

import Contract.Scripts (Validator)
import Contract.Transaction (plutusV2Script)
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Data.Argonaut (decodeJson)
import Data.Array (head)
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

