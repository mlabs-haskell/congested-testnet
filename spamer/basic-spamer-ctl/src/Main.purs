module Spamer.Main where


import Contract.Prelude

import Contract.Address (NetworkId(..), ownPaymentPubKeyHash)
import Contract.Chain (waitNSlots)
import Contract.Config (ContractParams, PrivatePaymentKeySource(..), WalletSpec(..), defaultKupoServerConfig, defaultOgmiosWsConfig, emptyHooks)
import Contract.Monad (launchAff_, liftedE, runContract)
import Contract.Numeric.Natural (fromInt')
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx, unspentOutputs)
import Contract.Transaction (balanceTx, getTxFinalFee, signTransaction, submitTxFromConstraints)
import Contract.TxConstraints (TxConstraints, mustPayToPubKey)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value (lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (QueryBackendParams(..))
import Ctl.Internal.Types.Int (toBigInt)
import Data.BigInt as BInt
import Data.UInt (fromInt)
import Effect.Exception (error)

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
    mOwnPkeyHash <- ownPaymentPubKeyHash 
    pKhash <- liftMaybe (error "no public key hash") mOwnPkeyHash 
    mUtxos <- getWalletUtxos
    utxos <- liftMaybe (error "no utxos") mUtxos
    let
        value = lovelaceValueOf (BInt.fromInt 13123456) 
        lookups :: ScriptLookups Void 
        lookups = unspentOutputs utxos 
        constraints :: TxConstraints Void Void 
        constraints = mustPayToPubKey pKhash value
    -- txId <- submitTxFromConstraints lookups constraints
    ubTx <- liftedE $ mkUnbalancedTx lookups constraints 
    -- _ <- waitNSlots (fromInt' 1) 
    bTx <- liftedE (balanceTx ubTx)
    -- bTx <- signTransaction =<< 
    -- log $ show $ getTxFinalFee bTx
    log "Contract here"  
  


