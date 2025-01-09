module Utils where

import Contract.Prelude
import Contract.Prelude

import Cardano.Serialization.Lib (Ed25519KeyHash)
import Cardano.Types (NetworkId(..))
import Cardano.Types (PaymentPubKeyHash, PrivateKey)
import Cardano.Types.BigNum (fromStringUnsafe)
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Promise (Promise, toAff)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.Array (findIndex, fromFoldable, unsafeIndex)
import Data.Array.ST as ST
import Data.List.Lazy (replicateM)
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Time.Duration (Seconds)
import Data.UInt (fromInt)
import Effect.Aff (delay, try)
import Effect.Class.Console (logShow)
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

type ParentPort = Foreign
type State = Foreign
type PostMsg = Foreign 
type RespType = String 

foreign import postP :: ParentPort -> PostMsg -> RespType -> State -> Promise Unit 
foreign import edHash :: String -> Ed25519KeyHash 
foreign import pKey  :: String -> PrivateKey 
foreign import delState  :: Foreign -> Effect Unit 
foreign import isEmptyState  :: Foreign -> Boolean 
foreign import requestBackendPars :: Foreign -> Effect Unit 
foreign import requestTx   :: Foreign -> Effect Unit 


makeTransaction :: ParentPort -> State -> TxPars -> Contract Unit  
makeTransaction pport state txPars 
  | isEmptyState state = log "nothing to do" 

  | txPars.tx == "initWallets" = do   
    let
      pars :: {hashes :: Array String, amount :: String}
      pars = unsafeFromForeign txPars.pars 
      pkhs = (wrap <<< wrap <<< edHash) <$> pars.hashes
    txHash <- payToWallets pars.amount pkhs 
    log $ "init spammer wallets : " <> (show txHash) 
    awaitTxConfirmedWithTimeout (wrap 100000.0) txHash
    -- response to server
    liftAff $ toAff $ postP pport (unsafeToForeign "initializedWallets") "OK" state 

  |txPars.tx == "pay" = do   
    let
      pars :: {key :: String, hash :: String, amount :: String}
      pars = unsafeFromForeign txPars.pars 
      keyWallet = privateKeysToKeyWallet (wrap $ pKey pars.key) Nothing Nothing       
      pkhs = (wrap <<< wrap <<< edHash) <$> (pure pars.hash)
    logShow pars.hash
    txHash <- withKeyWallet keyWallet $ payToWallets pars.amount pkhs 
    log $ "pay : " <> (show txHash) 
    -- response to server
    -- liftAff $ toAff $ postP pport (unsafeToForeign "paid") "OK" state 

  | otherwise = pure unit   



executeTransactionLoop :: ParentPort -> State -> Effect Unit
executeTransactionLoop pport state = launchAff_ do
   toAff $ postP pport (unsafeToForeign "reqBackendPars") "BackendPars" state 
   let
     env :: BackendPars
     env = unsafeFromForeign state
   runContract (contractParams env) do
      forever do
        liftAff $ toAff $ postP pport (unsafeToForeign "reqNextTransaction") "TxPars" state 
        -- let
        --   txPars :: TxPars
        --   txPars = unsafeFromForeign state
        -- log (unsafeFromForeign txPars.pars).hash
        -- makeTransaction pport state txPars 
        -- liftEffect $ delState state
        liftAff $ delay (wrap 1000.0)


payToWallets :: String -> Array PaymentPubKeyHash -> Contract TransactionHash
payToWallets amount pkhs = do
  let constraints = mconcat $ map (\pkh -> mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe amount)) pkhs
  txHash <- submitTxFromConstraints mempty constraints
  pure txHash


payFromKeyToPkh :: KeyWallet -> PaymentPubKeyHash  -> Contract TransactionHash
payFromKeyToPkh key pkh = do
  withKeyWallet key do
    payToWallets "3000000" (pure pkh)


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

