module Spamer.Main where


import Contract.Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (ContractParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (launchAff_, runContract)
import Contract.Utxos (getWalletBalance)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Data.UInt (fromInt)


main :: Effect Unit
main =launchAff_ do
  let 
      config :: ContractParams 
      config =
            { backendParams: CtlBackendParams 
              { ogmiosConfig: defaultOgmiosWsConfig 
              , kupoConfig: defaultKupoServerConfig {path = Nothing, port = fromInt 1442}
              } Nothing
            , networkId: TestnetId 
            , logLevel: Trace
            , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile "../../tmp/user1.skey") Nothing  
            , customLogger: Nothing
            , suppressLogs : false 
            , hooks : emptyHooks
            }
  runContract config do
    x <- getWalletBalance 
    log $ show x
    log "Contract here"  
  


