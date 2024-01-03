module Spammer.Contracts.GetFunds where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Config (ContractParams)
import Contract.Monad (launchAff_, runContract)
import Contract.Transaction (submitTxFromConstraints)
import Contract.Value (lovelaceValueOf)
import Ctl.Internal.Types.TxConstraints (mustPayToPubKey)
import Data.BigInt as BInt
import Effect.Aff (try)
import Spammer.Config (config)

config' :: ContractParams
config' = config "/faucet/wallet.skey" "ogmios.local" "kupo.local" 1337 1442

getFundsFromFaucet :: PaymentPubKeyHash -> Effect Unit
getFundsFromFaucet pkh = do
  launchAff_ do
    res <- try $ runContract config' do
      let
        constraints = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 1_000_000_000)
      _ <- submitTxFromConstraints mempty constraints
      pure unit
    either (log <<< show) (\_ -> pure unit) res

