module Spammer.Query.Scripts where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.Address (PaymentPubKeyHash(..), PubKeyHash(..))
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Monad (Contract, liftContractAffM)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (PlutusScript(..), Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, plutusV1Script, plutusV2Script, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Ctl.Internal.Types.Scripts (Language(..))
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Data.BigInt as BInt
import Data.Map (findMax)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Spammer.Db (executeQuery)
import Spammer.Keys (getEd25519HashFromPubKeyHex)
import Spammer.Utils (decodeCborHexToBytes)
import Spammer.Utils (decodeCborHexToBytes, liftJsonDecodeError)


type Result = Array { hex :: String }


getValidator :: Contract Validator 
getValidator = liftContractAffM
  "failed to get validator from spammer-db"
  do
    json <- executeQuery "SELECT hex FROM scripts ORDER BY time ASC LIMIT 1;"
    result :: Result <- liftEffect $ liftJsonDecodeError (decodeJson json)
    let
      res = do 
         bytes <- (_.hex <$> head result) >>= decodeCborHexToBytes 
         pure $ wrap <<< PlutusScript $ (bytes /\ PlutusV2)
    pure res

