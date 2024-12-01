module Spammer where

import Contract.Prelude

import Cardano.Types (Ed25519KeyHash(..), PaymentPubKeyHash, PrivateKey)
import Cardano.Types.BigNum (fromStringUnsafe)
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Promise (Promise, toAff)
import Data.Array (fromFoldable, unsafeIndex)
import Data.Array.ST as ST
import Data.List.Lazy (replicateM)
import Effect.Aff (joinFiber, launchAff, try)
import Effect.Class.Console (logShow)
import Effect.Random (random)
import Effect.Ref as RF
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Scripts (alwaysTrueScripts, payToAlwaysSucceeds, spendFromAlwaysSucceeds)
import Spammer.Config (config, getEnvVars)


payToWallets :: String -> Array PaymentPubKeyHash -> Contract TransactionHash 
payToWallets amount pkhs = do
  let constraints = mconcat $ map (\pkh -> mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe amount)) pkhs
  txHash <- submitTxFromConstraints mempty constraints 
  logShow txHash
  pure txHash


payFromKeyToPkh :: KeyWallet -> PaymentPubKeyHash -> Contract TransactionHash
payFromKeyToPkh  key pkh = do 
   withKeyWallet key do  
      payToWallets "3000000" (pure pkh) 


foreign import paidToSpammerWalletsSuccess  :: Foreign -> Promise Unit  
foreign import pauseSpammer :: Foreign -> Promise Unit 
foreign import addTxHash :: Foreign -> String -> Effect Unit
foreign import ed25519KeyHash :: Foreign -> Ed25519KeyHash 


getFundsFromFaucet :: Foreign -> Effect Unit 
getFundsFromFaucet obj = do 
  envVars <- getEnvVars
  let 
    params = config envVars
    ed25519hash = ed25519KeyHash obj

  fiber <- launchAff do 
     runContract params do 
        log "here"
        logShow ed25519hash  
        txHash <- payToWallets "1000000000" (pure (wrap ed25519hash))
        liftEffect (addTxHash obj (show txHash))
        pure txHash
  pure unit




spammer :: Foreign -> Effect Unit  
spammer controlVars = do 
  envVars <- getEnvVars
  let 
    params = config envVars
    nWallets = 200
    maxNScriptLock = 100
  scripts <- alwaysTrueScripts
  let nScripts = length scripts
  -- generate random wallets
  privKeys :: Array PrivateKey <- fromFoldable <$> replicateM nWallets generate 
  let 
    keys :: Array KeyWallet  
    keys = map (\privKey -> privateKeysToKeyWallet (wrap privKey) Nothing Nothing) privKeys  
    pkhs :: Array PaymentPubKeyHash 
    pkhs = map (\privKey -> wrap $ hash $ toPublicKey $ privKey) privKeys 

  launchAff_ do 
     -- send tada to spammer wallets
     runContract params do 
       liftAff $ toAff $ pauseSpammer controlVars
       log "pay to spammer wallets..."
       txHash <- payToWallets "1000000000000" pkhs 
       awaitTxConfirmedWithTimeout (wrap 100000.0) txHash
       log "paid to spammer wallets"
       -- notify about full spammer wallets to fill wallets in other spammers
       liftAff $ toAff $ paidToSpammerWalletsSuccess controlVars
       log "wait other spammers activation"
       liftAff $ toAff $ pauseSpammer controlVars
       log "infinite spammer loop"
       iWallet <- liftEffect $ RF.new 0
       iScript <- liftEffect $ RF.new 0
       nLocked <- liftEffect $ RF.new 0 
       -- mutable array with tuples of locked txHash and script id
       lockedTxScriptId :: ST.STArray Global (TransactionHash /\ Int) <- liftEffect $ toEffect ST.new 
       -- spammer loop
       forever do 
          randomNumber <- liftEffect $ random
          iW <- liftEffect $ RF.read iWallet
          nL <- liftEffect $ RF.read nLocked 
          let key  = unsafePartial $ unsafeIndex keys iW

          let 
              iWNext | iW == nWallets - 1 = 0
              iWNext = iW + 1 
       -- choose between lock unlock or just pay to wallet 
          let 
              makeTransaction randNum _ 
                | randNum < 0.4 = do
                   let pkh  = unsafePartial $ unsafeIndex pkhs iWNext
                   _ <- try $ payFromKeyToPkh key pkh 
                   pure unit
              makeTransaction _ nLock 
              -- lock transaction
                | nLock < maxNScriptLock = do
                   iS <- liftEffect $ RF.read iScript
                   let iSNext | iS == nScripts - 1 = 0
                       iSNext = iS + 1 
                   let _ /\ scriptHash  = unsafePartial $ unsafeIndex scripts iS
                   eitherTxHash <- try $ withKeyWallet key $ payToAlwaysSucceeds scriptHash   
                   case eitherTxHash of
                       Left e -> logShow e 
                       Right txHash' -> do 
                         newLen <- liftEffect $ toEffect $ ST.push (txHash' /\ iS) lockedTxScriptId
                         liftEffect $ RF.write newLen nLocked
                         log "locked successfully"
                   liftEffect $ RF.write iSNext iScript

              -- unlock transaction
                | nLock >= maxNScriptLock = do
                   -- extract first element from array
                   maybeTuple <- liftEffect $ toEffect $ ST.shift lockedTxScriptId
                   case maybeTuple of
                       -- Nothing -> pure unit
                       Nothing -> log "no elements in locked transactions" 
                       Just (txHash' /\ iS) -> do 
                         liftEffect $ RF.modify_ (_-1) nLocked
                         let script /\ scriptHash  = unsafePartial $ unsafeIndex scripts iS
                         eitherTxHash <- try $ withKeyWallet key $ spendFromAlwaysSucceeds scriptHash script txHash'
                         case eitherTxHash of
                             Right _ -> logShow "unlock successfully" 
                             Left e -> do
                               newLen <- liftEffect $ toEffect $ ST.push (txHash' /\ iS) lockedTxScriptId
                               liftEffect $ RF.write newLen nLocked
                               logShow e
              makeTransaction _ _ = pure unit 


          makeTransaction randomNumber nL  
          -- next wallet
          liftEffect $ RF.write iWNext iWallet

