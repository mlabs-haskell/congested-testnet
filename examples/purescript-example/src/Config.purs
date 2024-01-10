module Config (config) where

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
      -- As clarified in Eternl discord, they synchronize with the server every 2
      -- minutes, so 125 seconds would probably be enough.
      -- For other wallets, it is not very important
      { delay: Milliseconds 1_000.0, timeout: Seconds 125.0 }
  , syncBackend:
      -- Operations are costly, so the delay is 3 set to seconds
      { delay: Milliseconds 3_000.0, timeout: Seconds 120.0 }
  , awaitTxConfirmed:
      -- CIP-30 calls are cheap, so the delay can be just 1 second
      { delay: Milliseconds 1_000.0, timeout: Seconds 100.0}
  , waitUntilSlot: { delay: Milliseconds 1_000.0 }
  }

-- | Default synchronization parameters with all synchronization primitives enabled. `errorOnTimeout` options are all set to `false`.
-- | See `doc/query-layers.md` for more info.
defaultSynchronizationParams :: ContractSynchronizationParams
defaultSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false, beforeCip30Methods: true, beforeBalancing: true }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: true }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: true }
  }

config :: String -> String -> String -> Int -> Int -> ContractParams
config walletPath ogmiosHost kupoHost ogmiosPort kupoPort =
  { backendParams: CtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig { host = ogmiosHost, port = fromInt ogmiosPort }
      , kupoConfig: defaultKupoServerConfig { host = kupoHost, port = fromInt kupoPort, path = Nothing }
      }
      Nothing
  , networkId: TestnetId
  , logLevel: Info 
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile walletPath) Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }

