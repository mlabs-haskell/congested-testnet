module Spammer.Config (getEnvVars, EnvVars, config) where

import Contract.Prelude

import Cardano.Types (NetworkId(..))
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)


type EnvVars =
  { walletPath :: String
  , ogmiosUrl  :: String
  , kupoUrl :: String
  }

foreign import getEnvVars :: Effect EnvVars 

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


config :: EnvVars -> ContractParams
config envVars =
  { backendParams: CtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig { host = envVars.ogmiosUrl, port = fromInt 1337}
      , kupoConfig: defaultKupoServerConfig { host = envVars.kupoUrl, port = fromInt 1442, path = Nothing }
      }
      Nothing
  , networkId: TestnetId 
  , logLevel: Info 
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile $ envVars.walletPath) Nothing Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }

