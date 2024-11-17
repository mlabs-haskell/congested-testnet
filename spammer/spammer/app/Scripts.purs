module Scripts where

import Contract.Prelude (type (/\), Effect, Maybe(..), Unit, bind, discard, hush, join, liftM, mempty, pure, show, unwrap, wrap, ($), (/\), (<$>), (<<<), (<>), (>>>))
import Spammer.Config (config, getEnvVars)

import Cardano.Transaction.Builder (DatumWitness(DatumValue), OutputWitness(PlutusScriptOutput), ScriptWitness(ScriptValue), TransactionBuilderStep(SpendOutput, Pay))
import Cardano.Types (Credential(PubKeyHashCredential, ScriptHashCredential), Language(..), PaymentCredential(PaymentCredential), PlutusScript, ScriptHash, StakeCredential(StakeCredential), TransactionOutput(TransactionOutput))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (hash)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, lookupTxHash, submitTxFromBuildPlan)
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Wallet (ownStakePubKeyHashes)
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Types.Cbor (toByteArray)
import Data.Array (head)
import Data.ByteArray (hexToByteArray)
import Data.Map as Map
import Effect.Class.Console (logShow)
import Effect.Exception (error)

foreign import alwaysSucceeds :: Array String

main :: Effect Unit
main = do
  envVars <- getEnvVars
  let 
    params = config envVars
  launchAff_ do
     runContract params $ do 
        scriptStr <- liftContractM "can't read string script" $ head alwaysSucceeds
        logShow $ scriptStr
        script <- liftContractM "can't decode script from string" $ decodeCborHexToScript scriptStr 
        let scriptHash = hash script
        txHash <- payToAlwaysSucceeds scriptHash 
        awaitTxConfirmedWithTimeout (wrap 1000.0) txHash
        logShow txHash
        txHash2 <- spendFromAlwaysSucceeds scriptHash script txHash 
        awaitTxConfirmedWithTimeout (wrap 1000.0) txHash2
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
  -> Contract TransactionHash
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
  logInfo' "Successfully spent locked values."
  pure $ Transaction.hash spendTx



decodeCborHexToScript :: String -> Maybe PlutusScript 
decodeCborHexToScript cborHex = do
  cborBa <- hexToByteArray cborHex
  ba <- hush $ toByteArray $ wrap $ wrap cborBa
  pure $ wrap $ ba /\ PlutusV2


alwaysTrueScripts :: Effect (Array (PlutusScript /\ ScriptHash))
alwaysTrueScripts = 
  let
      extract scriptStr = unsafeFromJust "wrong script code" $ decodeCborHexToScript scriptStr  
      scriptAndHash script = script /\ (hash script)
  in
  pure $ (extract >>> scriptAndHash) <$> alwaysSucceeds 
