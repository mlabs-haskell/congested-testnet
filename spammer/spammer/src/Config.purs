module Spammer.Config (config) where


import Contract.Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (ContractParams, ContractSynchronizationParams, ContractTimeParams, PrivatePaymentKey(..), PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt (fromInt)
import Spammer.Keys (getPrivateKeyFromHex)




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
      { delay: Milliseconds 1_000.0, timeout: Seconds infinity}
  , waitUntilSlot: { delay: Milliseconds 1_000.0 }
  }

defaultSynchronizationParams :: ContractSynchronizationParams 
defaultSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false, beforeCip30Methods: true, beforeBalancing: true }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: true }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: true }
  }

config :: ContractParams 
config =
  let
      pkey = PrivatePaymentKey (getPrivateKeyFromHex "815f8272944a975701c7aa7e81969673e9a7973ce7b15b911be9bd4304f94139")
   in
      { backendParams: CtlBackendParams 
      { ogmiosConfig: defaultOgmiosWsConfig {host = "127.0.0.1"} 
        , kupoConfig: defaultKupoServerConfig {path = Nothing, port = fromInt 1442}
        } Nothing
      , networkId: TestnetId 
      , logLevel: Debug 
      -- , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile "../../tmp/wallet0.skey") Nothing  
      , walletSpec: Just $ UseKeys (PrivatePaymentKeyValue pkey) Nothing  
      , customLogger: Nothing
      , suppressLogs : false 
      , hooks : emptyHooks
      , timeParams: defaultTimeParams 
      , synchronizationParams: defaultSynchronizationParams
      }

