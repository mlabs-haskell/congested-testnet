module Example where
{- 
  In this example, we use congested testnet and purescript for simple transaction.

  1. To get test ada use next command inside `congested-testnet` repo 
      ```
      nix run .#get-tada  

      ```
      it will generate a pair of keys and request ADA from the faucet.
  2. install spago with purescript, or use inside this repo 
      ```
      nix develop .#purs

      ```
      it enters nix shell with necessary tools

      run ```npm intall``` to install necessary npm modules 

  3. inside purescript-example folder now we should have key.skey file generated in (1)  

  4. inside purescript-example run 
     ```
      spago run -m Example

     ```
     it will submit and wait tx confirmation
 
-}

import Contract.Monad (ContractParams, launchAff_, liftContractM, runContract)
import Contract.Prelude (Effect, LogLevel(..), Maybe(..), Unit, bind, discard, liftEffect, log, mempty, show, wrap, ($), (<>))

import Cardano.Types (NetworkId(..))
import Cardano.Types.BigNum (fromStringUnsafe)
import Contract.Config (ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (ContractParams, launchAff_, liftContractM, runContract)
import Contract.Transaction (awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (ownPaymentPubKeyHash)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Time.Duration (Seconds)
import Data.UInt (fromInt)
import Effect.Class.Console (logShow)
import Effect.Now (nowTime)

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

config :: ContractParams
config =
  { backendParams: CtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig { host = "congested-testnet.staging.mlabs.city", port = fromInt 1337 }
      , kupoConfig: defaultKupoServerConfig { host = "congested-testnet.staging.mlabs.city", port = fromInt 1442, path = Nothing }
      }
      Nothing
  , networkId: TestnetId
  , logLevel: Info
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile $ "key.skey") Nothing Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }



main ::  Effect Unit
main = do
  launchAff_ do
    runContract config do
      mPkh <- ownPaymentPubKeyHash
      pkh <- liftContractM "no pkh" mPkh  
      let constraint = mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe "3000000") 
      txHash <- submitTxFromConstraints mempty constraint
      start <- liftEffect nowTime
      awaitTxConfirmedWithTimeout (wrap 10000.0) txHash
      end <- liftEffect nowTime
      let
        dt :: Seconds
        dt = diff end start
      log $ " ============ measure tx time =============== time:" <> (show dt)
      logShow txHash

