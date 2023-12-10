module Contracts.Utils where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Utxos (UtxoMap)
import Contract.Wallet (getWalletUtxos)
import Data.Array (take)
import Data.Map (fromFoldable, toUnfoldable)

getInputUtxos :: Maybe UtxoMap -> Contract (Maybe UtxoMap)
getInputUtxos mutxos = do
  case mutxos of
    x@(Just _) -> pure x
    Nothing -> do
      mutxos' <- getWalletUtxos
      log $ show mutxos'
      log $ show $ (fromFoldable <<< take 1 <<< toUnfoldable) <$> mutxos'
      pure do
        utxos' <- mutxos'
        pure <<< fromFoldable <<< take 1 <<< toUnfoldable $ utxos'

