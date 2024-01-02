module Spammer.Contracts.GetFunds where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Config (ContractParams)
import Contract.Monad (launchAff_, runContract)
import Contract.Transaction (submitTxFromConstraints)
import Contract.Value (lovelaceValueOf)
import Ctl.Internal.Types.TxConstraints (mustPayToPubKey)
import Spammer.Config (config)
import Data.BigInt as BInt

config' :: ContractParams
config' = config "/tmp/faucet/wallet.skey" "localhost" "localhost" 1337 1442

getFundsFromFaucet :: PaymentPubKeyHash -> Effect Unit
getFundsFromFaucet pkh = do
  launchAff_ do
    runContract config' do
      let
        constraints = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 1_000_000_000)
      _ <- submitTxFromConstraints mempty constraints
      pure unit

