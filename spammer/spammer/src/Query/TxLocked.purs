module Spammer.Query.TxLocked where

import Contract.Prelude

import Contract.Monad (liftContractAffM, Contract)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionHash, plutusV2Script)
import Ctl.Internal.Types.Transaction (TransactionInput(..))
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Data.UInt (fromInt)
import Spammer.Db (executeQuery)
import Spammer.Query.Utils (liftJsonDecodeError, quotes)


insertTxLocked :: TransactionHash -> String -> String -> Contract Unit
insertTxLocked txHash txOutInd valId = liftContractAffM "error insert txlocked" do
  let
    txHashString = byteArrayToHex <<< unwrap $ txHash
    query' = "INSERT INTO txlocked (txHash, txOutInd, valId, time) VALUES ("
      <> quotes txHashString
      <> ","
      <> txOutInd
      <> ","
      <> valId
      <> ",NOW());"
  _ <- executeQuery query'
  pure <<< pure $ unit


type Result = Array { txhash :: String, txoutind :: Int, hex :: String}

getTxLocked :: Contract {txLocked :: TransactionInput, validator :: Validator} 
getTxLocked = liftContractAffM "error get txlocked" do
  let
    query' = """
      SELECT txHash, txOutInd, encode(validator, 'hex') as hex 
      FROM txlocked LEFT JOIN validators ON valId=id   
      ORDER BY time ASC LIMIT 1;
    """
  json <- executeQuery query'
  result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
  pure do
    x <- head result
    bytes <- hexToByteArray x.txhash
    vbytes <- hexToByteArray x.hex
    let validator = wrap <<<  plutusV2Script $ vbytes
        transactionId = wrap bytes
        index = fromInt x.txoutind 
    pure $ {txLocked : TransactionInput {index, transactionId}, validator}

