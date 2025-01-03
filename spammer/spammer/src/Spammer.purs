module Spammer where

import Contract.Prelude

import Cardano.Types (Ed25519KeyHash, PaymentPubKeyHash, PrivateKey)
import Cardano.Types.BigNum (fromStringUnsafe)
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Transaction (TransactionHash, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Promise (Promise, toAff)
import Data.Array (findIndex, fromFoldable, unsafeIndex)
import Data.Array.ST as ST
import Data.List.Lazy (replicateM)
import Data.Time (diff)
import Data.Time.Duration (Seconds)
import Effect.Aff (delay, try)
import Effect.Class.Console (logShow)
import Effect.Now (nowTime)
import Effect.Random (random)
import Effect.Ref as RF
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Scripts (alwaysTrueScripts, distribution, payToAlwaysSucceeds, spendFromAlwaysSucceeds)
import Spammer.Config (config, getEnvVars)

payToWallets :: String -> Array PaymentPubKeyHash -> Contract TransactionHash
payToWallets amount pkhs = do
  let constraints = mconcat $ map (\pkh -> mustPayToPubKey pkh (lovelaceValueOf $ fromStringUnsafe amount)) pkhs
  txHash <- submitTxFromConstraints mempty constraints
  logShow txHash
  pure txHash

payFromKeyToPkh :: KeyWallet -> PaymentPubKeyHash -> Contract TransactionHash
payFromKeyToPkh key pkh = do
  withKeyWallet key do
    payToWallets "3000000" (pure pkh)

foreign import paidToSpammerWalletsSuccess :: Foreign -> Promise Unit
foreign import pauseSpammer :: Foreign -> Promise Unit
foreign import addTxHash :: Foreign -> String -> Effect Unit
foreign import ed25519KeyHash :: Foreign -> Effect Ed25519KeyHash
foreign import allowTx :: Foreign -> Boolean
foreign import updateLastTime :: Seconds -> Foreign -> Effect Unit
foreign import spammerId :: Foreign -> Int
foreign import isWaitTx :: Foreign -> Boolean 
foreign import isPayFaucet :: Foreign -> Effect Boolean 
foreign import sendTxHash :: String -> Foreign -> Effect Unit 
foreign import getIWallet ::  Foreign -> Effect Int 
foreign import putIWallet :: Int -> Foreign -> Effect Unit 
foreign import resetKeyHash :: Foreign -> Effect Unit 
foreign import payLovelace :: Foreign -> Effect String 

faucet :: Foreign -> Effect Unit
faucet obj = do
  envVars <- getEnvVars
  let
    -- generate many wallets for faucet in order to handle many requests 
    nWallets = 100
    params = config envVars
  privKeys :: Array PrivateKey <- fromFoldable <$> replicateM nWallets generate
  let
    keys :: Array KeyWallet
    keys = map (\privKey -> privateKeysToKeyWallet (wrap privKey) Nothing Nothing) privKeys

    pkhs :: Array PaymentPubKeyHash
    pkhs = map (\privKey -> wrap $ hash $ toPublicKey $ privKey) privKeys

  launchAff_ do
    runContract params do
      log "pay to faucet wallets..."
      txHash' <- payToWallets "1000000000000" pkhs
      awaitTxConfirmedWithTimeout (wrap 100000.0) txHash'
      liftAff $ toAff $ paidToSpammerWalletsSuccess obj
      let
          iWNext x | x == nWallets - 1 = 0
          iWNext x = x + 1

          payOrWait true  = do
            iW <- liftEffect $ getIWallet obj
            ed25519hash <- liftEffect $ ed25519KeyHash obj 
            let key = unsafePartial $ unsafeIndex keys iW
            payAmount' <- liftEffect $ payLovelace obj
            txHash <- withKeyWallet key $ payToWallets payAmount' (pure (wrap ed25519hash))
            liftEffect $ sendTxHash (show txHash) obj
            liftEffect $ putIWallet (iWNext iW) obj
            liftEffect $ resetKeyHash obj 

          payOrWait false = do 
             liftAff $ delay (wrap 100.0)


      forever do
         x <- liftEffect $ isPayFaucet obj
         payOrWait x



spammer :: Foreign -> Effect Unit
spammer controlVars = do
  log " ============ start spammer =============== "
  envVars <- getEnvVars
  
  let
    spammerId' = spammerId controlVars
    params = config envVars
    nWallets = 200
    maxNScriptLock = 100
    waitTx = isWaitTx controlVars

    awaitOrMoveForwardTx :: Either _ TransactionHash -> Contract Unit
    awaitOrMoveForwardTx eTxHash = do
       case eTxHash of
           Left e -> logShow e
           Right txHash | waitTx -> do 
              start <- liftEffect nowTime
              awaitTxConfirmedWithTimeout (wrap 10000.0) txHash
              end <- liftEffect nowTime
              let
                dt :: Seconds
                dt = diff end start
              log $ " ============ measure tx time =============== time:" <> (show dt)
              liftEffect $ updateLastTime dt controlVars
           Right _ -> do 
              pure unit

  scripts <- alwaysTrueScripts
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
      txHash' <- payToWallets "1000000000000" pkhs
      awaitTxConfirmedWithTimeout (wrap 100000.0) txHash'
      -- notify about full spammer wallets to fill wallets in other spammers
      liftAff $ toAff $ paidToSpammerWalletsSuccess controlVars
      iWallet <- liftEffect $ RF.new 0
      nLocked <- liftEffect $ RF.new 0
      -- mutable array with tuples of locked txHash and script id
      lockedTxScriptId :: ST.STArray Global (TransactionHash /\ Int) <- liftEffect $ toEffect ST.new
      -- spammer loop
      forever do
        randomNumber <- liftEffect $ random
        iW <- liftEffect $ RF.read iWallet
        nL <- liftEffect $ RF.read nLocked
        let key = unsafePartial $ unsafeIndex keys iW

        let
          iWNext | iW == nWallets - 1 = 0
          iWNext = iW + 1
        -- choose between lock unlock or just pay to wallet 
        let
          selectScriptIndBasedOnDistr :: Number -> Maybe Int
          selectScriptIndBasedOnDistr x = findIndex (x > _) distribution

          maybeIScript = selectScriptIndBasedOnDistr randomNumber

          -- skip transaction
          makeTransaction _ _ false = do
            liftAff $ delay (wrap 10.0)

          -- pay to wallet / simple transaction
          makeTransaction Nothing _ _ = do
            let pkh = unsafePartial $ unsafeIndex pkhs iWNext
            eTxHash <- try $ payFromKeyToPkh key pkh
            awaitOrMoveForwardTx eTxHash

          -- unlock transaction
          makeTransaction _ nLock _
            | nLock >= maxNScriptLock = do
                -- extract first element from array
                maybeTuple <- liftEffect $ toEffect $ ST.shift lockedTxScriptId
                case maybeTuple of
                  -- Nothing -> pure unit
                  Nothing -> log "no elements in locked transactions"
                  Just (txHash /\ iS) -> do
                    liftEffect $ RF.modify_ (_ - 1) nLocked
                    let script /\ scriptHash = unsafePartial $ unsafeIndex scripts iS
                    eitherTxHash <- try $ withKeyWallet key $ spendFromAlwaysSucceeds scriptHash script txHash
                    awaitOrMoveForwardTx eitherTxHash 
                    case eitherTxHash of
                      Right _ -> do
                        log $ "unlock successfully" <> show spammerId'
                      Left e -> do
                        newLen <- liftEffect $ toEffect $ ST.push (txHash' /\ iS) lockedTxScriptId
                        liftEffect $ RF.write newLen nLocked
                        logShow e

          -- lock transaction
          makeTransaction (Just iS) nLock _
            | nLock < maxNScriptLock = do
                let _ /\ scriptHash = unsafePartial $ unsafeIndex scripts iS
                eitherTxHash <- try $ withKeyWallet key $ payToAlwaysSucceeds scriptHash
                awaitOrMoveForwardTx eitherTxHash 
                case eitherTxHash of
                  Left e -> logShow e
                  Right txHash -> do
                    newLen <- liftEffect $ toEffect $ ST.push (txHash /\ iS) lockedTxScriptId
                    liftEffect $ RF.write newLen nLocked
                    log $ "locked successfully" <> show spammerId'

          makeTransaction _ _ _ = pure unit

        makeTransaction maybeIScript nL (allowTx controlVars)
        -- next wallet
        liftEffect $ RF.write iWNext iWallet
