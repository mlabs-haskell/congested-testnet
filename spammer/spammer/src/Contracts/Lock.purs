module Spammer.Contracts.Lock where

import Contract.Prelude

import Contract.Address (NetworkId(..), scriptHashAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, validator)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, plutusV1Script, plutusV2Script, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..), TxConstraints, mustPayToPubKey, mustPayToScript, mustSpendPubKeyOutput, mustSpendScriptOutput)
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletUtxos, withKeyWallet)
import Contracts.Utils (getInputUtxos)
import Control.Alternative (guard)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.RWS (get)
import Control.Monad.State (StateT)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Ctl.Internal.Serialization.Types (BigInt, TransactionHash)
import Data.Array (head, take)
import Data.BigInt as BInt
import Data.Map (fromFoldable, keys, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Set (findMax)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Exception (error)
import Spammer.Query.Scripts (getValidator)
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..))
import Spammer.Utils (decodeCborHexToBytes)

newtype LockParams = LockParams
  { wallet :: KeyWallet
  , validator :: Validator
  , value :: Value
  , mutxos :: Maybe UtxoMap
  }

derive instance Newtype LockParams _
derive instance Generic LockParams _

extractLockPars :: SpammerEnv -> Maybe LockParams
extractLockPars (SpammerEnv env) = do
  wallet <- env.wallet
  validator <- env.validator
  value <- env.value
  pure <<< LockParams $ { wallet, validator, value, mutxos: env.utxos }

lock :: StateT SpammerEnv Contract Unit
lock = do
  env <- get
  let mpars = extractLockPars env
  case mpars of
    Nothing -> pure unit
    Just (LockParams pars) -> do
      lift $ withKeyWallet pars.wallet do
        mutxos <- getInputUtxos pars.mutxos
        case mutxos of
          Nothing -> pure unit
          Just utxos -> do
            txInput <- liftMaybe (error "no txId") (findMax $ keys utxos)
            ahash <- ownPubKeyHashes
            pHash <- liftMaybe (error "no phash") (head ahash)
            let
              lookups = unspentOutputs utxos <> validator pars.validator
              -- lookups = validator pars.validator
              valHash = validatorHash pars.validator
              constraints =
                -- mustPayToScript valHash unitDatum DatumWitness pars.value <>
                  mustSpendPubKeyOutput txInput
                  <>
                    mustPayToPubKey (wrap pHash) (lovelaceValueOf $ BInt.fromInt 1_000_000)
            log $ show utxos
            txId <- submitTxFromConstraints lookups constraints
            pure unit

