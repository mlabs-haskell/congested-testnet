module Spammer.Contracts.Unlock where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, plutusV1Script, plutusV2Script, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, withKeyWallet)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.RWS (get)
import Control.Monad.State (StateT)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Serialization.Types (BigInt, TransactionHash)
import Data.Array (take)
import Data.BigInt as BInt
import Data.Map (findMax, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.Wallet (getWallet')
import Spammer.Types (SpammerEnv(..))
import Spammer.Utils (decodeCborHexToBytes)

newtype UnlockParams = UnlockParams {
  wallet :: KeyWallet , 
  validator :: Validator 
  }

derive instance Newtype UnlockParams _
derive instance Generic UnlockParams _

unlock :: UnlockParams -> StateT SpammerEnv Contract Unit 
unlock (UnlockParams pars) = lift $ withKeyWallet pars.wallet do
  mUtxos <- getWalletUtxos
  let 
      utxos = do
        utxos <- mUtxos
        pure <<< fromFoldable <<< take 1 <<< toUnfoldable $ utxos 

  case utxos of  
      Nothing -> pure unit 
      Just utxos -> do 
        allScriptUtxos <- utxosAt (scriptHashAddress (validatorHash pars.validator) Nothing)
        selectedScriptUtxo <- liftMaybe (error "can't find any script utxo") (findMax allScriptUtxos)
        let
            -- selectedScriptUtxos = fromFoldable <<< take 1 <<< toUnfoldable $ allScriptUtxos 
            lookups = unspentOutputs allScriptUtxos <> validator pars.validator
            constraints = mustSpendScriptOutput (selectedScriptUtxo.key) unitRedeemer

        log $ show utxos
        txId <- submitTxFromConstraints lookups constraints
        pure unit

