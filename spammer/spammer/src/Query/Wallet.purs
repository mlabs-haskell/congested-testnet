module Spammer.Query.Wallet where

import Contract.Prelude

import Contract.Monad (Contract, liftContractAffM)
import Contract.Wallet (KeyWallet, PrivatePaymentKey(..), privateKeysToKeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Data.Argonaut (decodeJson)
import Data.Array (head)
import Data.BigInt (BigInt, toString)
import Effect.Exception (error)
import Spammer.Db (executeQuery)
import Spammer.Keys (genPrivateKey, getEdHash, getHexFromEd25519Hash, getPrivateKeyFromHex, getPrivateKeyHex, getPubKeyHashHex, getPubKeyHex)
import Spammer.Query.Utils (liftJsonDecodeError, quotes)

type PrivKeyQueryResult = Array
  { pkey :: String }

type CountNullWallets = Array
  { count :: Number
  }

getWallet' :: Aff (Maybe KeyWallet)
getWallet' = do
  let
    query' =
      """    
                 WITH cte AS (
                      SELECT pkey 
                      FROM wallets 
                      WHERE time = (SELECT MIN(time) FROM wallets)
                      LIMIT 1
                  )
                   UPDATE wallets 
                   SET time = NOW() 
                   FROM cte
                   WHERE wallets.pkey = cte.pkey
                   RETURNING encode(wallets.pkey, 'hex') as pkey;
               """
  json <- executeQuery query'
  result :: PrivKeyQueryResult <- liftEffect $ liftJsonDecodeError (decodeJson json)
  pure do
    x <- head result
    let
      pkey = PrivatePaymentKey (getPrivateKeyFromHex x.pkey)
      keyWallet = privateKeysToKeyWallet pkey Nothing
    pure keyWallet

genNewPubKeyHash :: Contract PaymentPubKeyHash
genNewPubKeyHash = do
  newPrivKey <- liftEffect genPrivateKey
  let edHash = getEdHash newPrivKey
  pure <<< wrap <<< wrap $ edHash

generateNewWalletDb :: Contract Unit
generateNewWalletDb = do
  newPrivKey <- liftEffect genPrivateKey
  let
    newPrivKeyHex = getPrivateKeyHex newPrivKey
    newPubKeyHex = getPubKeyHex newPrivKey
    newPubKeyHashHex = getPubKeyHashHex newPrivKey

    insertNewKeyDb = "INSERT INTO wallets (pkey, pubkey, pubkeyhash, time)"
      <> " SELECT "
      <> quotes newPrivKeyHex
      <> ","
      <> quotes newPubKeyHex
      <> ","
      <> quotes newPubKeyHashHex
      <> ", null, NOW() "
      <> " FROM (SELECT 1) as FOO "
      <> "WHERE (SELECT COUNT(pkey) FROM pkeys WHERE balance < 1) = 0;"

  _ <- liftContractAffM "cannot get keyWallet" $ pure <$> executeQuery insertNewKeyDb
  pure unit

countNullWallets :: Contract Number
countNullWallets = do
  let
    query' = "SELECT COUNT(pkey) as count FROM pkeys WHERE balance IS NULL;"
  json <- liftContractAffM "cannot count null wallets" $ pure <$> executeQuery query'
  result :: CountNullWallets <- liftEffect $ liftJsonDecodeError (decodeJson json)
  res <- liftMaybe (error "empty array in getWallet'") $ head result
  pure res.count

updateWalletBalanceDb :: PaymentPubKeyHash -> BigInt -> Contract Unit
updateWalletBalanceDb phash value = do
  let
    phashhex = getHexFromEd25519Hash <<< unwrap <<< unwrap $ phash
    query' = "UPDATE pkeys SET balance = " <> toString value
      <> ", time = NOW() "
      <> " WHERE pubkeyhash = "
      <> quotes phashhex
      <> " ;"
  _ <- liftContractAffM "cannot get keyWallet" $ pure <$> executeQuery query'
  pure unit
