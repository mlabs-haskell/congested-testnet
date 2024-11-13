module Scripts where

import Contract.Prelude
import Contract.Prelude
import Spammer.Config

import Cardano.Serialization.Lib (privateKey_generateEd25519, privateKey_toBech32, privateKey_toPublic, publicKey_hash)
import Cardano.Transaction.Builder (DatumWitness(DatumValue), OutputWitness(PlutusScriptOutput), ScriptWitness(ScriptValue), TransactionBuilderStep(SpendOutput, Pay))
import Cardano.Types (BigInt, BigNum(..), Language(..), NetworkId(..), PaymentPubKeyHash(..), PrivateKey(..))
import Cardano.Types (Credential(PubKeyHashCredential, ScriptHashCredential), PaymentCredential(PaymentCredential), PlutusScript, ScriptHash, StakeCredential(StakeCredential), TransactionHash, TransactionOutput(TransactionOutput))
import Cardano.Types.Address (toBech32)
import Cardano.Types.BigNum (fromBigInt, fromInt, fromStringUnsafe)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (hash)
import Cardano.Types.PlutusScript as Script
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, KnownWallet(Nami), WalletSpec(ConnectToGenericCip30), testnetConfig, walletName)
import Contract.Config (PrivatePaymentKeySource(..), PrivateStakeKeySource(..), WalletSpec(..), ContractParams)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.Monad (Contract, launchAff_, runContract, runContractInEnv, withContractEnv)
import Contract.Prim.ByteArray (ByteArray(..))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (TransactionHash(..), awaitTxConfirmed, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.Transaction (awaitTxConfirmed, lookupTxHash, submitTxFromBuildPlan)
import Contract.TxConstraints (mustPayToPubKey, mustPayToPubKeyAddress)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Value as Value
import Contract.Wallet (KeyWallet, Wallet(..), ownPaymentPubKeyHash, privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet (ownStakePubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Rec.Class (Step(..), forever, tailRec)
import Control.Safely (replicateM_)
import Ctl.Internal.Contract.Wallet (withWallet)
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Types.Cbor (toByteArray)
import Ctl.Internal.Wallet (Wallet(..), mkKeyWallet)
import Ctl.Internal.Wallet.Spec (mkWalletBySpec)
import Data.Array (fromFoldable, head, replicate, slice, unsafeIndex, zip)
import Data.Array (head)
import Data.Array (replicate)
import Data.ByteArray (hexToByteArray)
import Data.List.Lazy (List, replicateM)
import Data.Map as Map
import Data.Typelevel.Undefined (undefined)
import Effect.AVar (AVar)
import Effect.Aff (delay, error, forkAff, try)
import Effect.Aff.AVar (new, take, tryPut, tryTake)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as RF
import Partial.Unsafe (unsafePartial)

foreign import alwaysSucceeds :: Array String

main :: Effect Unit
main = do
  envVars <- getEnvVars
  let 
    params = config envVars
  launchAff_ do
     runContract params $ do 
        scriptStr <- liftContractM "can't extract" $ head alwaysSucceeds
        script <- liftContractM "can't extract" $ decodeCborHexToScript scriptStr 
        let scriptHash = hash script
        txHash <- payToAlwaysSucceeds scriptHash 
        awaitTxConfirmedWithTimeout (wrap 100.0) txHash
        txHash2 <- spendFromAlwaysSucceeds scriptHash script txHash 
        logShow txHash
        logShow txHash2 


payToAlwaysSucceeds :: ScriptHash -> Contract TransactionHash
payToAlwaysSucceeds vhash = do
  -- Send to own stake credential. This is used to test mustPayToScriptAddress.
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress (PaymentCredential $ ScriptHashCredential vhash)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
  Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
        , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
        , scriptRef: Nothing
        }
    ]

spendFromAlwaysSucceeds
  :: ScriptHash
  -> PlutusScript
  -> TransactionHash
  -> Contract Unit
spendFromAlwaysSucceeds vhash validator txId = do
  -- Use own stake credential if available
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress
    (wrap $ ScriptHashCredential vhash)
    (wrap <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
  utxos <- utxosAt scriptAddress
  utxo <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
      )
      $ head (lookupTxHash txId utxos)
  spendTx <- submitTxFromBuildPlan (Map.union utxos $ toUtxoMap [ utxo ])
    mempty
    [ SpendOutput
        utxo
        ( Just $ PlutusScriptOutput (ScriptValue validator) RedeemerDatum.unit
            $ Just
            $ DatumValue
            $ PlutusData.unit
        )
    ]
  awaitTxConfirmed $ Transaction.hash spendTx
  logInfo' "Successfully spent locked values."



decodeCborHexToScript :: String -> Maybe PlutusScript 
decodeCborHexToScript cborHex = do
  cborBa <- hexToByteArray cborHex
  ba <- hush $ toByteArray $ wrap $ wrap cborBa
  pure $ wrap $ ba /\ PlutusV2


-- alwaysSucceedsScript :: Contract PlutusScript
-- alwaysSucceedsScript = do
--   liftMaybe (error "Error decoding alwaysSucceeds") do
--     envelope <- decodeTextEnvelope alwaysSucceeds
--     plutusScriptFromEnvelope envelope

-- decodeTextEnvelope json = do
--   aeson <- hush $ parseJsonStringToAeson json
--   { "type": type_, description, cborHex } <-
--     hush $ decodeAeson aeson :: _ TextEnvelopeRaw
--   ba <- decodeCborHexToBytes cborHex
