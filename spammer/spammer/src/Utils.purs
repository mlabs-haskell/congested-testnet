module Utils where

import Contract.Prelude

import Cardano.Serialization.Lib (Ed25519KeyHash, TransactionHash) as CSL
import Cardano.Transaction.Builder (DatumWitness(DatumValue), OutputWitness(PlutusScriptOutput), ScriptWitness(ScriptValue), TransactionBuilderStep(SpendOutput, Pay))
import Cardano.Types (Credential(PubKeyHashCredential, ScriptHashCredential), Language(..), PaymentCredential(PaymentCredential), PlutusScript, ScriptHash, StakeCredential(StakeCredential), TransactionOutput(TransactionOutput))
import Cardano.Types (NetworkId(..))
import Cardano.Types (PaymentPubKeyHash, PrivateKey)
import Cardano.Types.BigNum (fromStringUnsafe)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (hash) as PScript
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, lookupTxHash, submitTxFromBuildPlan)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Value as Value
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet (ownStakePubKeyHashes)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Promise (Promise, toAff, toAffE)
import Control.Promise (toAffE)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Types.Cbor (toByteArray)
import Data.Array (findIndex, fromFoldable, unsafeIndex)
import Data.Array (head)
import Data.Array.ST as ST
import Data.ByteArray (hexToByteArray)
import Data.List.Lazy (replicateM)
import Data.Map as Map
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Time.Duration (Seconds)
import Data.UInt (fromInt)
import Effect.Aff (delay, try)
import Effect.Class.Console (logShow)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Effect.Now (nowTime)
import Effect.Random (random)
import Effect.Ref as RF
import Foreign (F, Foreign, isNull, unsafeFromForeign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)


type BackendPars =
  { walletPath :: String
  , ogmiosUrl :: String
  , kupoUrl :: String
  }

type TxPars =
  { 
    tx :: String,
    pars :: Foreign 
  }

-- main :: Effect Unit
-- main = launchAff_ do
--   let
--       backendPars = { kupoUrl: "0.0.0.0", ogmiosUrl: "0.0.0.0", walletPath: "/home/maxim/work/projects/congested-testnet/containers/genesis_spo_data/wallet.skey" }
--   logShow backendPars
--   runContract (contractParams backendPars) do
--     log "here"


executeTransactionLoop :: Foreign -> Effect Unit
executeTransactionLoop parentPort = launchAff_ do
  backendPars :: BackendPars <- unsafeFromForeign <$> (toAffE $ requestParent parentPort "backendPars") 
  logShow backendPars 
  runContract (contractParams backendPars) do
     forever do
        txPars :: TxPars <- unsafeFromForeign <$> (liftAff <<< toAffE $ requestParent parentPort "txPars") 
        -- logShow txPars.tx
        res <- try $ makeTransaction txPars 
        resp' <- liftAff <<< toAffE $ case res of 
            Right resp ->  requestParent parentPort resp 
            Left e -> requestParent parentPort $ "Fail : " <> show e
        pure unit


foreign import requestParent :: Foreign -> String -> Effect (Promise Foreign) 
foreign import edHash :: String -> CSL.Ed25519KeyHash 
foreign import pKey  :: String -> PrivateKey 
foreign import txHashToHex :: CSL.TransactionHash -> String
foreign import txHashFromHex :: String -> CSL.TransactionHash


makeTransaction :: TxPars -> Contract String 
makeTransaction txPars 

  | txPars.tx == "initWallets" = do   
    let
      pars :: {hashes :: Array String, amount :: String}
      pars = unsafeFromForeign txPars.pars 
      pkhs = (wrap <<< wrap <<< edHash) <$> pars.hashes
    txHash <- payToWallets pars.amount pkhs 
    log $ "init spammer wallets : " <> (show txHash) 
    awaitTxConfirmedWithTimeout (wrap 100000.0) txHash
    pure "initializedWallets"

  | txPars.tx == "pay" = do   
    let
      pars :: {key :: String, hash :: String, amount :: String}
      pars = unsafeFromForeign txPars.pars 
      keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing       
      pkhs = (wrap <<< wrap <<< edHash) <$> (pure pars.hash)
    txHash <- withKeyWallet keyWallet $ payToWallets pars.amount pkhs 
    log $ "pay : " <> (show txHash) 
    pure "paid"


  | txPars.tx == "lock" = do   
    let
      pars :: {key :: String, script :: String, amount :: String}
      pars = unsafeFromForeign txPars.pars 
      script = unsafeFromJust "wrong script code" $ decodeCborHexToScript pars.script 
      scriptHash = PScript.hash script
      keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing       
    txHash <- withKeyWallet keyWallet $ payToAlwaysSucceeds scriptHash
    log $ "locked : " <> (show txHash) 
    pure $ "locked_" <> pars.script <> "_" <> (txHashToHex <<< unwrap $ txHash) 

  | txPars.tx == "unlock" = do   
    let
      pars :: {key :: String, script :: String, lockedTxHash :: String}
      pars = unsafeFromForeign txPars.pars 
      script = unsafeFromJust "wrong script code" $ decodeCborHexToScript pars.script 
      scriptHash = PScript.hash script
      txHash = wrap <<< txHashFromHex $ pars.lockedTxHash 
      keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing       
    txHash <- withKeyWallet keyWallet $ spendFromAlwaysSucceeds scriptHash script txHash
    log $ "unlocked : " <> (show txHash) 
    pure $ "unlocked_" <> pars.lockedTxHash 

  | otherwise = pure "" 



payToWallets :: String -> Array PaymentPubKeyHash -> Contract TransactionHash
payToWallets amount pkhs = do
  let constraints = mconcat $ map (\pkh -> mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe amount)) pkhs
  txHash <- submitTxFromConstraints mempty constraints
  pure txHash


payFromKeyToPkh :: KeyWallet -> PaymentPubKeyHash  -> Contract TransactionHash
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
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile $ pars.walletPath) Nothing Nothing
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





-- faucet :: Foreign -> Effect Unit
-- faucet obj = do
--   envVars <- getEnvVars
--   let
--     -- generate many wallets for faucet in order to handle many requests 
--     nWallets = 50
--     params = config envVars
--   privKeys :: Array PrivateKey <- fromFoldable <$> replicateM nWallets generate
--   let
--     keys :: Array KeyWallet
--     keys = map (\privKey -> privateKeysToKeyWallet (wrap privKey) Nothing Nothing) privKeys
--
--     pkhs :: Array PaymentPubKeyHash
--     pkhs = map (\privKey -> wrap $ hash $ toPublicKey $ privKey) privKeys
--
--   launchAff_ do
--     runContract params do
--       log "pay to faucet wallets..."
--       txHash' <- payToWallets "1000000000000" pkhs
--       awaitTxConfirmedWithTimeout (wrap 100000.0) txHash'
--       liftAff $ toAff $ paidToSpammerWalletsSuccess obj
--       let
--           iWNext x | x == nWallets - 1 = 0
--           iWNext x = x + 1
--
--           payOrWait true  = do
--             iW <- liftEffect $ getIWallet obj
--             ed25519hash <- liftEffect $ ed25519KeyHash obj 
--             let key = unsafePartial $ unsafeIndex keys iW
--             payAmount' <- liftEffect $ payLovelace obj
--             txHash <- withKeyWallet key $ payToWallets payAmount' (pure (wrap ed25519hash))
--             liftEffect $ sendTxHash (show txHash) obj
--             liftEffect $ putIWallet (iWNext iW) obj
--             liftEffect $ resetKeyHash obj 
--
--           payOrWait false = do 
--              liftAff $ delay (wrap 100.0)
--
--
--       forever do
--          x <- liftEffect $ isPayFaucet obj
--          payOrWait x

