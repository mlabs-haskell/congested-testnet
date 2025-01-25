module Worker where

import Contract.Prelude

import Cardano.Serialization.Lib (Ed25519KeyHash, TransactionHash) as CSL
import Cardano.Transaction.Builder (DatumWitness(DatumValue), OutputWitness(PlutusScriptOutput), ScriptWitness(ScriptValue), TransactionBuilderStep(SpendOutput, Pay))
import Cardano.Types (Credential(PubKeyHashCredential, ScriptHashCredential), Language(..), PaymentCredential(PaymentCredential), PlutusScript, ScriptHash, StakeCredential(StakeCredential), TransactionOutput(TransactionOutput), PaymentPubKeyHash, PrivateKey, NetworkId(..))
import Cardano.Types.BigNum (fromStringUnsafe)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (hash) as PScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, submitTxFromConstraints, lookupTxHash, submitTxFromBuildPlan)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Value as Value
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet, ownStakePubKeyHashes)
import Control.Monad.Rec.Class (forever)
import Control.Promise (Promise, toAffE)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Types.Cbor (toByteArray)
import Data.Array (head)
import Data.ByteArray (hexToByteArray)
import Data.Map as Map
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Effect.Aff (delay, try)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Effect.Now (nowTime)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)

type BackendPars =
  { mainWalletPath :: String
  , ogmiosUrl :: String
  , kupoUrl :: String
  }

type TxPars =
  { tx :: String
  , pars :: Foreign
  }

type TxResponse =
  { msg :: String
  , time :: String
  }

executeTransactionLoop :: Foreign -> Effect Unit
executeTransactionLoop parentPort = do 
  backendPars <- getBackendPars
  logShow backendPars
  launchAff_ do
    runContract (contractParams backendPars) do
      forever do
         txPars :: TxPars <- unsafeFromForeign <$> (liftAff <<< toAffE $ waitPars parentPort )
         msg <- try $ makeTransaction txPars 
         liftEffect 
          $ sendMsg parentPort 
           $ case msg of
                 Left e -> "error_" <> show e
                 Right m -> m
         liftAff $ delay $ wrap 10.0


foreign import edHash :: String -> CSL.Ed25519KeyHash
foreign import pKey :: String -> PrivateKey
foreign import txHashToHex :: CSL.TransactionHash -> String
foreign import txHashFromHex :: String -> CSL.TransactionHash
foreign import getBackendPars :: Effect BackendPars
foreign import waitPars :: Foreign -> Effect (Promise Foreign) 
foreign import sendMsg :: Foreign -> String -> Effect Unit 


makeTransaction :: TxPars -> Contract String 
makeTransaction txPars

  | txPars.tx == "initWallets" = do
      let
        pars :: { hashes :: Array String, amount :: String}
        pars = unsafeFromForeign txPars.pars
        pkhs = (wrap <<< wrap <<< edHash) <$> pars.hashes
      txHash <- payToWallets pars.amount pkhs
      log $ "init spammer wallets : " <> (show txHash)
      let msg = "initWallets_" <> "_" <> (txHashToHex <<< unwrap $ txHash)
      pure msg 

  | txPars.tx == "pay" = do
      let
        pars :: { key :: String, hash :: String, amount :: String}
        pars = unsafeFromForeign txPars.pars
        keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing
        pkhs = (wrap <<< wrap <<< edHash) <$> (pure pars.hash)
      txHash <- withKeyWallet keyWallet $ payToWallets pars.amount pkhs
      log $ "pay : " <> (show txHash)
      let msg = "paid_" <> pars.hash <> "_" <> (txHashToHex <<< unwrap $ txHash)
      pure msg

  | txPars.tx == "lock" = do
      let
        pars :: { key :: String, script :: String, amount :: String}
        pars = unsafeFromForeign txPars.pars
        script = unsafeFromJust "wrong script code" $ decodeCborHexToScript pars.script
        scriptHash = PScript.hash script
        keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing
      txHash <- withKeyWallet keyWallet $ payToAlwaysSucceeds scriptHash
      log $ "locked : " <> (show txHash)
      let msg = "locked_" <> pars.script <> "_" <> (txHashToHex <<< unwrap $ txHash)
      pure msg

  | txPars.tx == "unlock" = do
      let
        pars :: { key :: String, script :: String, lockedTxHash :: String}
        pars = unsafeFromForeign txPars.pars
        script = unsafeFromJust "wrong script code" $ decodeCborHexToScript pars.script
        scriptHash = PScript.hash script
        lockedTxHash = wrap <<< txHashFromHex $ pars.lockedTxHash
        keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing
      txHash <- withKeyWallet keyWallet $ spendFromAlwaysSucceeds scriptHash script lockedTxHash
      log $ "unlocked : " <> (show txHash)
      let msg = "unlocked_" <> pars.lockedTxHash <> "_" <> (txHashToHex <<< unwrap $ txHash)
      pure msg

  | otherwise = pure "" 

measureAwaitTxTime :: TransactionHash -> Contract String
measureAwaitTxTime txHash = do
  start <- liftEffect nowTime
  awaitTxConfirmedWithTimeout (wrap 2000.0) txHash
  end <- liftEffect nowTime
  let
    dt :: Seconds
    dt = diff end start
  pure $ show (unwrap dt)

payToWallets :: String -> Array PaymentPubKeyHash -> Contract TransactionHash
payToWallets amount pkhs = do
  let constraints = mconcat $ map (\pkh -> mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe amount)) pkhs
  txHash <- submitTxFromConstraints mempty constraints
  pure txHash

payFromKeyToPkh :: KeyWallet -> PaymentPubKeyHash -> Contract TransactionHash
payFromKeyToPkh key pkh = do
  withKeyWallet key do
    payToWallets "3000000" (pure pkh)

payToAlwaysSucceeds :: ScriptHash -> Contract TransactionHash
payToAlwaysSucceeds vhash = do
  -- Send to own stake credential. This is used to test mustPayToScriptAddress.
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress (PaymentCredential $ ScriptHashCredential vhash)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
  txHash <- Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
        , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
        , scriptRef: Nothing
        }
    ]
  pure txHash

spendFromAlwaysSucceeds
  :: ScriptHash
  -> PlutusScript
  -> TransactionHash
  -> Contract TransactionHash
spendFromAlwaysSucceeds vhash validator txId = do
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
  pure $ Transaction.hash spendTx

decodeCborHexToScript :: String -> Maybe PlutusScript
decodeCborHexToScript cborHex = do
  cborBa <- hexToByteArray cborHex
  ba <- hush $ toByteArray $ wrap $ wrap cborBa
  pure $ wrap $ ba /\ PlutusV2

contractParams :: BackendPars -> ContractParams
contractParams pars =
  { backendParams: CtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig { host = pars.ogmiosUrl, port = fromInt 1337 }
      , kupoConfig: defaultKupoServerConfig { host = pars.kupoUrl, port = fromInt 1442, path = Nothing }
      }
      Nothing
  , networkId: TestnetId
  , logLevel: Info
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile $ pars.mainWalletPath) Nothing Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }

defaultTimeParams :: ContractTimeParams
defaultTimeParams =
  { syncWallet:
      { delay: Milliseconds 1.0, timeout: Seconds 0.0 }
  , syncBackend:
      { delay: Milliseconds 1.0, timeout: Seconds 0.0 }
  , awaitTxConfirmed:
      { delay: Milliseconds 1.0, timeout: Seconds 0.0 }
  , waitUntilSlot: { delay: Milliseconds 1.0 }
  }

defaultSynchronizationParams :: ContractSynchronizationParams
defaultSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false, beforeCip30Methods: false, beforeBalancing: false }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: false }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: false }
  }

