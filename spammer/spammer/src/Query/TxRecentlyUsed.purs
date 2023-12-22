module Spammer.Query.TxRecentlyUsed where

import Contract.Prelude

import Contract.Monad (liftContractAffM, Contract)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Ctl.Internal.Types.Transaction (TransactionInput(..))
import Data.Argonaut (decodeJson)
import Data.Array as Array 
import Data.Set as Set
import Data.UInt (fromInt, toInt)
import Spammer.Db (executeQuery)
import Spammer.Query.Utils (bytea, liftJsonDecodeError)

insertTxRecentlyUsed :: Set.Set TransactionInput -> Contract Unit
insertTxRecentlyUsed txInputs = liftContractAffM "error insert txRecentlyUsed" do
  let
    arrInputs :: Array TransactionInput
    arrInputs = Set.toUnfoldable txInputs
    txHash x = byteArrayToHex <<< unwrap $ x
    value (TransactionInput { index, transactionId }) =
      "("
        <> (bytea <<< txHash $ transactionId)
        <> ","
        <> (show <<< toInt $ index)
        <> ","
        <> "NOW()"
        <>
          ")"

    query' = "INSERT INTO txRecentlyUsed (txHash, txOutInd, time) VALUES"
      <> (Array.fold <<< Array.intersperse "," $ value <$> arrInputs)
      <> " ON CONFLICT (txhash, txOutInd) DO NOTHING"
      <> ";"
  _ <- executeQuery query'
  pure <<< pure $ unit

type Result = Array { txhash :: String, txoutind :: Int }

getTxRecentlyUsed :: Contract (Set.Set TransactionInput) 
getTxRecentlyUsed = liftContractAffM "error get txRecentlyUsed" do
  let
    query' = """
        DELETE FROM txRecentlyUsed WHERE time < NOW() - INTERVAL '2 minute';  
        SELECT encode(txHash, 'hex') as txhash, txOutInd FROM txRecentlyUsed;
        """
  json <- executeQuery query'
  result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
  let
      toTransaction {txhash, txoutind} = 
        TransactionInput {index : fromInt txoutind, transactionId : wrap <<< hexToByteArrayUnsafe $ txhash}  
  pure <<< pure <<< Set.fromFoldable $ toTransaction <$> result




