module Spammer.Query.TxRecentlyUsed where

import Contract.Prelude

import Contract.Monad (liftContractAffM, Contract)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionHash(..), TransactionInput(..), plutusV2Script)
import Contract.Utxos (UtxoMap, getUtxo, utxosAt)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.Transaction (TransactionInput(..))
import Data.Argonaut (decodeJson)
import Data.Array (fold, head, intersperse)
import Data.Map (findMax)
import Data.Map.Internal (keys)
import Data.Set as Set
import Data.UInt (fromInt, toInt)
import Effect.Aff (error)
import Spammer.Db (executeQuery)
import Spammer.Query.Utils (bytea, liftJsonDecodeError, quotes)

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
      <> (fold <<< intersperse "," $ value <$> arrInputs)
      <> ";"
  _ <- executeQuery query'
  pure <<< pure $ unit
