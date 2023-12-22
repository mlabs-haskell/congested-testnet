module Spammer.Query.TxLocked where

import Contract.Prelude

import Contract.Monad (liftContractAffM, Contract)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionHash(..), plutusV2Script)
import Ctl.Internal.Types.Transaction (TransactionInput(..))
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Data.UInt (fromInt, toInt)
import Spammer.Db (executeQuery)
import Spammer.Query.Utils (bytea, liftJsonDecodeError)

insertTxLocked :: TransactionHash -> String -> String -> Contract Unit
insertTxLocked txHash txOutInd valId = liftContractAffM "error insert txlocked" do
  let
    txHashString = byteArrayToHex <<< unwrap $ txHash
    query' = "INSERT INTO txlocked (txHash, txOutInd, valId, time) VALUES ("
      <> bytea txHashString
      <> ","
      <> txOutInd
      <> ","
      <> valId
      <> ",NOW());"
  _ <- executeQuery query'
  pure <<< pure $ unit

type Result = Array { txhash :: String, txoutind :: Int, hex :: String }

getTxLocked :: Contract { txLocked :: TransactionInput, validator :: Validator }
getTxLocked = liftContractAffM "error get txlocked" do
  let
    query' =
      """
            WITH cte AS (
              SELECT txHash, txOutInd, encode(validator, 'hex') as hex 
              FROM txlocked LEFT JOIN validators ON valId=id   
              WHERE valId is not NULL
              ORDER BY txlocked.time ASC LIMIT 1
            )
            UPDATE txlocked 
            SET time = NOW()
            FROM cte
            WHERE txlocked.txhash = cte.txhash and txlocked.txoutind = cte.txoutind
            RETURNING encode(cte.txHash, 'hex') as txHash, cte.txoutind, cte.hex;
    """
  json <- executeQuery query'
  result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
  pure do
    x <- head result
    bytes <- hexToByteArray x.txhash
    vbytes <- hexToByteArray x.hex
    let
      validator = wrap <<< plutusV2Script $ vbytes
      transactionId = wrap bytes
      index = fromInt x.txoutind
    pure $ { txLocked: TransactionInput { index, transactionId }, validator }

clearTxLocked :: TransactionInput -> Contract Unit
clearTxLocked txInput = liftContractAffM "error clear txlocked" do
  let
    { index: txind, transactionId: TransactionHash txHashByte } = unwrap $ txInput
    query' = "DELETE FROM txlocked WHERE txHash="
      <> bytea (byteArrayToHex txHashByte)
      <> " AND txoutind="
      <> (show <<< toInt $ txind)
      <> ";"
  _ <- executeQuery query'
  pure $ pure unit
