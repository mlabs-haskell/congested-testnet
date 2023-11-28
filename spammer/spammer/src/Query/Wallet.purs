module Spammer.Query.Wallet (getWallet', updateWalletBalanceDb) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractAffM)
import Contract.Wallet (KeyWallet, PrivatePaymentKey(..), privateKeysToKeyWallet)
import Contract.Wallet.Key (keyWalletPrivatePaymentKey)
import Control.Monad.Error.Class (liftMaybe)
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Effect.Exception (error)
import Spammer.Db (executeQuery)
import Spammer.Keys (genPrivateKey, getPrivateKeyFromHex, getPrivateKeyHex, getPubKeyHex)
import Spammer.Utils (liftJsonDecodeError, quotes)

type PrivKeyQueryResult = Array
  { pkey :: String
  }

getWallet' :: Maybe KeyWallet -> Contract KeyWallet
getWallet' previousWalletWithError = liftContractAffM "cannot get keyWallet" do
  query' <- query previousWalletWithError
  json <- executeQuery query'
  result :: PrivKeyQueryResult <- liftEffect $ liftJsonDecodeError (decodeJson json)
  res <- liftMaybe (error "empty array in getWallet'") $ head result
  let
    pkey = PrivatePaymentKey (getPrivateKeyFromHex res.pkey)
    keyWallet = privateKeysToKeyWallet pkey Nothing
  pure <<< pure $ keyWallet

query :: Maybe KeyWallet -> Aff String
query previousWalletWithError = liftEffect do
  case previousWalletWithError of
    Just keyWallet -> do
      newPrivKey <- genPrivateKey
      let
        newPrivKeyHex = getPrivateKeyHex newPrivKey
        newPubKeyHex = getPubKeyHex newPrivKey
        prevPrivateKey = unwrap $ keyWalletPrivatePaymentKey keyWallet
        prevPrivateKeyHex = getPrivateKeyHex prevPrivateKey

        insertNewKeyDb = "INSERT INTO pkeys (pkey, pubkey, balance) VALUES "
          <> "( "
          <> quotes newPrivKeyHex
          <> ","
          <> quotes newPubKeyHex
          <> ",0); "

        selectKey = "SELECT pkey FROM pkeys "
          <> " WHERE pkey != "
          <>
            quotes prevPrivateKeyHex
          <>
            " ORDER BY balance DESC LIMIT 1;"

      pure $ insertNewKeyDb <> selectKey

    Nothing -> pure $ "SELECT pkey FROM pkeys ORDER BY balance DESC LIMIT 1;"

updateWalletBalanceDb :: KeyWallet -> Contract Unit
updateWalletBalanceDb keyWallet = do
  pure unit
