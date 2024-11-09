module Spammer where

import Contract.Prelude
import Spammer.Config

import Cardano.Serialization.Lib (privateKey_generateEd25519, privateKey_toBech32, privateKey_toPublic, publicKey_hash)
import Cardano.Types (BigInt, BigNum(..), NetworkId(..), PaymentPubKeyHash(..), PrivateKey(..))
import Cardano.Types.Address (toBech32)
import Cardano.Types.BigNum (fromBigInt, fromInt, fromStringUnsafe)
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Contract.Config (PrivatePaymentKeySource(..), PrivateStakeKeySource(..), WalletSpec(..), ContractParams)
import Contract.Monad (Contract, launchAff_, runContract, runContractInEnv, withContractEnv)
import Contract.Transaction (TransactionHash(..), awaitTxConfirmed, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey, mustPayToPubKeyAddress)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet, Wallet(..), ownPaymentPubKeyHash, privateKeysToKeyWallet, withKeyWallet)
import Control.Monad.Rec.Class (Step(..), forever, tailRec)
import Control.Safely (replicateM_)
import Ctl.Internal.Contract.Wallet (withWallet)
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Wallet (Wallet(..), mkKeyWallet)
import Ctl.Internal.Wallet.Spec (mkWalletBySpec)
import Data.Array (fromFoldable, replicate, slice, unsafeIndex, zip)
import Data.Array (replicate)
import Data.List.Lazy (List, replicateM)
import Data.Typelevel.Undefined (undefined)
import Effect.AVar (AVar)
import Effect.Aff (delay, error, forkAff, try)
import Effect.Aff.AVar (new, take, tryPut, tryTake)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as RF
import Partial.Unsafe (unsafePartial)

generateNWallets :: Int -> Effect ((Array KeyWallet) /\ (Array PaymentPubKeyHash))
generateNWallets nWallets = do
    privKeys :: Array PrivateKey <- fromFoldable <$> replicateM nWallets generate 
    let
      keyWallets :: Array KeyWallet 
      keyWallets = map (\privKey -> privateKeysToKeyWallet (wrap privKey) Nothing Nothing) privKeys  
      pubHashes :: Array PaymentPubKeyHash 
      pubHashes = map (\privKey -> wrap $ hash $ toPublicKey $ privKey) privKeys 
    pure $ keyWallets /\ pubHashes


payToWallet :: String -> Array PaymentPubKeyHash -> Contract TransactionHash 
payToWallet amount pkhs = do
  let constraints = mconcat $ map (\pkh -> mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe amount)) pkhs
  txHash <- submitTxFromConstraints mempty constraints 
  logShow txHash
  pure txHash


payFromKeyToPkh :: KeyWallet -> PaymentPubKeyHash -> Contract TransactionHash
payFromKeyToPkh  key pkh = do 
   withKeyWallet key do  
      payToWallet "3000000" (pure pkh) 


main :: Effect Unit
main = do
  envVars <- getEnvVars
  let 
    params = config envVars
    -- nWallets in each spammer
    nWallets = 200
    nSpammers = 2 
  keysAndPkhsForSpammers :: Array (Tuple (Array _) (Array _))  <- fromFoldable <$> replicateM nSpammers (generateNWallets nWallets)
  -- mutable wallet indicies for spammer loop
  walletIndss :: Array (Ref _)  <- fromFoldable <$> replicateM nSpammers (RF.new (0 /\ 1))
  launchAff_ do
     -- fill wallets for each spammer
     runContract params $ do 
        let
            payToAllSpammerWallets pkhs = do 
              txHash <- payToWallet "1000000000000" pkhs 
              awaitTxConfirmedWithTimeout (wrap 100.0) txHash
        sequence_ $ map (\(_ /\ pkhs) -> payToAllSpammerWallets pkhs) keysAndPkhsForSpammers 

     -- spammers are group of wallets   
     -- infinite loop where in each spammer funds moves cyclic from one wallet to other
     let 
         args = zip keysAndPkhsForSpammers walletIndss

         spammerLoop :: _ -> Aff _ 
         spammerLoop ((keys /\ pkhs) /\ walletInds) = 
           forkAff $ runContract params $ forever do
              (i :: Int) /\ j <- liftEffect $ RF.read walletInds 
              logShow i
              let
                 iterate (i /\ _) | i == (nWallets - 2) = 1 /\ 0 
                 iterate (i /\ _) | i == (nWallets - 1) = 0 /\ 1 
                 iterate (i /\ j) = (i + 2) /\ (j + 2)

                 key = unsafePartial $ unsafeIndex keys i
                 pkh = unsafePartial $ unsafeIndex pkhs j

              _ <- try $ payFromKeyToPkh key pkh 
              _ <- liftEffect $ RF.modify iterate walletInds 
              pure unit

     sequence_ $ map spammerLoop args 
     pure unit



    



