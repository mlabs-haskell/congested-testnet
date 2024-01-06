module Spammer.Config (config) where

import Contract.Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)

defaultTimeParams :: ContractTimeParams
defaultTimeParams =
  { syncWallet:
      { delay: Milliseconds 1.0, timeout: Seconds 0.0}
  , syncBackend:
      { delay: Milliseconds 1.0, timeout: Seconds 0.0}
  , awaitTxConfirmed:
      { delay: Milliseconds 1.0, timeout: Seconds 0.0}
  , waitUntilSlot: { delay: Milliseconds 1.0 }
  }

defaultSynchronizationParams :: ContractSynchronizationParams
defaultSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false, beforeCip30Methods: false , beforeBalancing: false }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: false }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: false }
  }

config :: String -> String -> String -> Int -> Int -> ContractParams
config walletPath ogmiosHost kupoHost ogmiosPort kupoPort =
  { backendParams: CtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig { host = ogmiosHost, port = fromInt ogmiosPort }
      , kupoConfig: defaultKupoServerConfig { host = kupoHost, port = fromInt kupoPort, path = Nothing }
      }
      Nothing
  , networkId: TestnetId
  , logLevel: Debug
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile walletPath) Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }

