module SpammerUtils where

import Contract.Prelude
import Spammer.Config

import Cardano.Serialization.Lib (PlutusScripts, privateKey_generateEd25519, privateKey_toBech32, privateKey_toPublic, publicKey_hash)
import Cardano.Transaction.Builder (DatumWitness(DatumValue), OutputWitness(PlutusScriptOutput), ScriptWitness(ScriptValue), TransactionBuilderStep(SpendOutput, Pay))
import Cardano.Types (BigInt, BigNum(..), NetworkId(..), PaymentPubKeyHash(..), PrivateKey(..), ScriptHash(..))
import Cardano.Types (Credential(PubKeyHashCredential, ScriptHashCredential), PaymentCredential(PaymentCredential), PlutusScript, ScriptHash, StakeCredential(StakeCredential), TransactionHash, TransactionOutput(TransactionOutput))
import Cardano.Types.Address (toBech32)
import Cardano.Types.BigNum (fromBigInt, fromInt, fromStringUnsafe)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as Script
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, KnownWallet(Nami), WalletSpec(ConnectToGenericCip30), testnetConfig, walletName)
import Contract.Config (PrivatePaymentKeySource(..), PrivateStakeKeySource(..), WalletSpec(..), ContractParams)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Monad (Contract, launchAff_, runContract, runContractInEnv, withContractEnv)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (TransactionHash(..), awaitTxConfirmed, awaitTxConfirmedWithTimeout, submitTxFromConstraints)
import Contract.Transaction (awaitTxConfirmed, lookupTxHash, submitTxFromBuildPlan)
import Contract.TxConstraints (mustPayToPubKey, mustPayToPubKeyAddress)
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Value as Value
import Contract.Wallet (KeyWallet, Wallet(..), ownPaymentPubKeyHash, privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet (ownStakePubKeyHashes)
import Control.Alternative (guard)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Rec.Class (Step(..), forever, tailRec)
import Control.Monad.ST (ST, run)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Safely (replicateM_)
import Ctl.Internal.Contract.Wallet (withWallet)
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Wallet (Wallet(..), mkKeyWallet)
import Ctl.Internal.Wallet.Spec (mkWalletBySpec)
import Data.Array (fromFoldable, head, modifyAt, replicate, slice, unsafeIndex, zip, length)
import Data.Array.ST as ST
import Data.List.Lazy (List, replicateM)
import Data.Map as Map
import Data.Typelevel.Undefined (undefined)
import Effect.AVar (AVar)
import Effect.Aff (delay, error, forkAff, joinFiber, launchAff, never, try)
import Effect.Aff.AVar as AVAR
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as RF
import Partial.Unsafe (unsafePartial)
import Scripts (alwaysTrueScripts, payToAlwaysSucceeds, spendFromAlwaysSucceeds)


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



spammer :: Effect Unit  
spammer = do 
  envVars <- getEnvVars
  let 
    params = config envVars
    nWallets = 200
    maxNScriptLock = 200
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
     log "init spammer wallets"
     -- send tada to spammer wallets
     runContract params do 
       txHash <- payToWallets "1000000000000" pkhs 
       awaitTxConfirmedWithTimeout (wrap 1000.0) txHash
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
                       Left _ -> pure unit 
                       Right txHash -> do 
                         newLen <- liftEffect $ toEffect $ ST.push (txHash /\ iS) lockedTxScriptId
                         liftEffect $ RF.write newLen nLocked
                   liftEffect $ RF.write iSNext iScript

              -- unlock transaction
                | nLock >= maxNScriptLock = do
                   -- extract first element from array
                   maybeTuple <- liftEffect $ toEffect $ ST.shift lockedTxScriptId
                   case maybeTuple of
                       Nothing -> pure unit
                       Just (txHash /\ iS) -> do 
                         liftEffect $ RF.modify_ (_-1) nLocked
                         let script /\ scriptHash  = unsafePartial $ unsafeIndex scripts iS
                         eitherTxHash <- try $ withKeyWallet key $ spendFromAlwaysSucceeds scriptHash script txHash   
                         case eitherTxHash of
                             Right _ -> pure unit 
                             Left _ -> do
                               newLen <- liftEffect $ toEffect $ ST.push (txHash /\ iS) lockedTxScriptId
                               liftEffect $ RF.write newLen nLocked
              makeTransaction _ _ = pure unit 


          makeTransaction randomNumber nL  
          -- next wallet
          liftEffect $ RF.write iWNext iWallet





-- checkMut :: Effect Unit
-- checkMut = do
  -- msg <- RF.read refStr
  -- log "hi" 


-- checkMut :: Ref String -> Effect Unit
-- checkMut refStr  = do
--   launchAff_ $ forever do
--     msg <- liftEffect $ RF.read refStr
--     log msg
--     delay (wrap 0.0)


-- checkMutArr :: Ref  -> Effect Unit
-- checkMut refStr  = do
--   launchAff_ $ forever do
--     msg <- liftEffect $ RF.read refStr
--     log msg
--     delay (wrap 0.0)
