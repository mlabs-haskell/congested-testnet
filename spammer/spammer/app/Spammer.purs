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
import Data.Array (fromFoldable, replicate, slice, unsafeIndex)
import Data.Array (replicate)
import Data.List.Lazy (List, replicateM)
import Data.Typelevel.Undefined (undefined)
import Effect.AVar (AVar)
import Effect.Aff (delay, error, forkAff, try)
import Effect.Aff.AVar (new, take, tryPut, tryTake)
import Effect.Class.Console (logShow)
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
      payToWallet "4000000" (pure pkh) 




main :: Effect Unit
main = do
  envVars <- getEnvVars
  let 
    params = config envVars
    nWallets = 200
  keys1 /\ pkhs1 <- generateNWallets nWallets 
  keys2 /\ pkhs2 <- generateNWallets nWallets 
  keys3 /\ pkhs3 <- generateNWallets nWallets 
  loopArgs1 <- RF.new (0 /\ 1)
  loopArgs2 <- RF.new (0 /\ 1)
  loopArgs3 <- RF.new (0 /\ 1)
  let 
      iterate (i /\ _) | i == (nWallets - 2) = 1 /\ 0 
      iterate (i /\ _) | i == (nWallets - 1) = 0 /\ 1 
      iterate (i /\ j) = (i + 2) /\ (j + 2)

      loop keys pkhs loopArgs = do 
        i /\ j <- liftEffect $ RF.read loopArgs 
        logShow i
        let 
            key = unsafePartial $ unsafeIndex keys i
            pkh = unsafePartial $ unsafeIndex pkhs j
        _ <- try $ payFromKeyToPkh key pkh 
        _ <- liftEffect $ RF.modify iterate loopArgs 
        pure unit



  launchAff_ do
     runContract params $ do 
         txHash <- payToWallet "1000000000000" pkhs1 
         awaitTxConfirmedWithTimeout (wrap 100.0) txHash
         txHash <- payToWallet "1000000000000" pkhs2 
         awaitTxConfirmedWithTimeout (wrap 100.0) txHash
         txHash <- payToWallet "1000000000000" pkhs3 
         awaitTxConfirmedWithTimeout (wrap 100.0) txHash

     _ <- forkAff $ runContract params do
         _ <- forever $ loop keys1 pkhs1 loopArgs1 
         pure unit
     _ <- forkAff $ runContract params do
         _ <- forever $ loop keys2 pkhs2 loopArgs2 
         pure unit
     _ <- forkAff $ runContract params do
         _ <- forever $ loop keys3 pkhs3 loopArgs3 
         pure unit
     pure unit



    

  -- launchAff_ $ withContractEnv params  \env -> do 
    -- runContractInEnv env $ paySelf 100 2_000_000
    -- replicateM_ 100 $ runContractInEnv env $ paySelf 1 8_000_000
  -- launchAff_ $ runContract params  do 
  --   paySelf 100 2_000_000
  --   replicateM_ 100 $ paySelf 1 2_000_000
  -- launchAff_ do
  --    runContract params $ paySelf "init" 100 2_000_000 
  --    _ <- forkAff $ runContract params  do 
  --       replicateM_ 100 $ paySelf "a" 1 2_500_000
  --    _ <- forkAff $ runContract params  do replicateM_ 100 $ paySelf "b" 1 3_000_000
  --    delay $ wrap 10000.0 


paySelf ::String -> Int -> Int -> Contract Unit
paySelf id nUtxos amount = do
  mpkh <- ownPaymentPubKeyHash
  pkh  <- liftM (error "no pkh") mpkh
  let constraints = mconcat (replicate nUtxos $ mustPayToPubKey pkh (lovelaceValueOf $ fromInt amount))
  txHash <- submitTxFromConstraints mempty constraints 
  log id
  log $ show txHash


     -- walletPayFromInd :: AVar Int <- new 0 
     -- walletPayToInd :: AVar Int <- new 1 
     -- _ <- forkAff $ runContract (config envVars) do 
     -- _ <- runContractInEnv env myContract1
        
     -- _ <- forkAff $ payFromWalletToWallet walletPayFromInd walletPayToInd 
     -- _ <- forkAff $ helper1 ax rx 
     -- ax <- new 100
     -- pure unit
     -- _ <- forkAff $ helper ax rx
     -- _ <- forkAff $ helper1 ax rx 
     -- delay $ wrap 1000.0
     -- log "finish"

-- data Parameters = PayFromWalletToWallet {keyWallets} 

payFromWalletToWallet :: ContractParams -> AVar Int -> AVar Int -> Aff Unit 
payFromWalletToWallet  pars walletPayFromInd walletPayToInd  = runContract pars  do
  pure unit




helper :: AVar Int -> RF.Ref Int -> Aff Unit
helper ax rx = forever do
  mx <- tryTake ax
  x  <- liftEffect $ RF.read rx
  logShow x
  case mx of
      Just y -> logShow y  
      Nothing -> do  
         delay (wrap 100.0) 
         logShow "no var" 



helper1 :: AVar Int -> RF.Ref Int -> Aff Unit
helper1 ax rx = forever do
  ifM (tryPut 200 ax) (pure unit) $ do
     delay (wrap 100.0) 
     logShow "can't put"

     -- runContract cfg do 
  -- pure unit
  -- priv_key <- generate 
  -- let pkh :: PaymentPubKeyHash 
  --     pkh  = PaymentPubKeyHash $ hash $ toPublicKey $ priv_key
  -- envVars <- getEnvVars
  -- log envVars.kupoUrl
  -- log envVars.walletPath
  -- let cfg = config envVars 
  -- launchAff_ $ do 
  --    runContract cfg do 
  --       let paySelf = mustPayToPubKey pkh (lovelaceValueOf $ fromInt 5_000_000)   
  --       txHash <- submitTxFromConstraints mempty paySelf
  --       log $ show txHash
  --       pure unit 


-- spreadFundsOnAddresses :: List PrivateKey -> Contract Unit
-- spreadFundsOnAddresses privKeys  = do
--   launchAff_ $ do 
--      runContract cfg do 
--         let paySelf = mustPayToPubKey pkh (lovelaceValueOf $ fromInt 5_000_000)   
--         txHash <- submitTxFromConstraints mempty paySelf
--         log $ show txHash
--         pure unit 

