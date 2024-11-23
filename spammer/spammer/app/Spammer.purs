module Spammer where

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
import Data.Array as ARR
import Data.Array.ST as ST
import Data.List.Lazy (List, replicateM)
import Data.Map as Map
import Data.Typelevel.Undefined (undefined)
import Effect.AVar (AVar)
import Effect.Aff (delay, error, forkAff, never, try)
import Effect.Aff.AVar as AVAR
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as RF
import Partial.Unsafe (unsafePartial)
import Scripts (alwaysTrueScripts, payToAlwaysSucceeds, spendFromAlwaysSucceeds)


generateNWallets :: Int -> Effect ((Array KeyWallet) /\ (Array PaymentPubKeyHash))
generateNWallets nWallets = do
    privKeys :: Array PrivateKey <- fromFoldable <$> replicateM nWallets generate 
    let
      keyWallets :: Array KeyWallet 
      keyWallets = map (\privKey -> privateKeysToKeyWallet (wrap privKey) Nothing Nothing) privKeys  
      pubHashes :: Array PaymentPubKeyHash 
      pubHashes = map (\privKey -> wrap $ hash $ toPublicKey $ privKey) privKeys 
    pure $ keyWallets /\ pubHashes


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


data Wait = Wait

data ScriptMode = Unlock | Lock

type SpammerArgs = { 
  wait :: AVAR.AVar Wait, 
  nWallets :: Int, 
  backendPars :: ContractParams,
  scripts :: Array (PlutusScript /\ ScriptHash),
  lockedTxHashes :: Array (AVAR.AVar TransactionHash),
  scriptInd :: AVAR.AVar Int, 
  scriptMode :: AVAR.AVar ScriptMode
}

spammer :: SpammerArgs -> Aff Unit  
spammer args = do 
  -- generate random wallets
  privKeys :: Array PrivateKey <- fromFoldable <$> replicateM args.nWallets (liftEffect  generate) 
  let 
    keys :: Array KeyWallet  
    keys = map (\privKey -> privateKeysToKeyWallet (wrap privKey) Nothing Nothing) privKeys  
    pkhs :: Array PaymentPubKeyHash 
    pkhs = map (\privKey -> wrap $ hash $ toPublicKey $ privKey) privKeys 
    -- lockedTxHashes :: Array (AVAR.AVar TransactionHash)

  -- init all wallets first, use sync primitive to avoid collision with other spammers 
  log "init spammer wallets"
  wait' <- AVAR.take args.wait
  runContract args.backendPars do 
    txHash <- payToWallets "1000000000000" pkhs 
    awaitTxConfirmedWithTimeout (wrap 1000.0) txHash
  _ <- AVAR.put wait' args.wait 

  -- create mutable ref for forever loop
  walletInd <- liftEffect $ RF.new 0 

  log "infinite loop"
  _ <- runContract args.backendPars $ forever do 
     i <- liftEffect $ RF.read walletInd
     let key = unsafePartial $ unsafeIndex keys i 
         next x | x == args.nWallets - 1 = 0
         next x  = x + 1 
     randomNumber <- liftEffect $ random
     if randomNumber < 0.01 
     -- simple transaction - pay to wallet
     then do
       let
         pkh = unsafePartial $ unsafeIndex pkhs (next i)
       _ <- try $ payFromKeyToPkh key pkh 
       pure unit
     -- transaction with a script interaction
     else do
       scriptMode :: ScriptMode <- liftAff $ AVAR.take args.scriptMode
       scriptInd :: Int <- liftAff $ AVAR.take args.scriptInd
       let 
         nScripts = length args.scripts 
         update Lock i | i == nScripts - 1 = Unlock /\ 0
         update Unlock i | i == nScripts - 1 = Lock /\ 0
         update x i  = x /\ (i + 1)
         scriptModeNext /\ scriptIndNext = update scriptMode scriptInd
       void $ liftAff $ AVAR.put scriptModeNext args.scriptMode
       void $ liftAff $ AVAR.put scriptIndNext args.scriptInd

       let
         script /\ scriptHash = unsafePartial $ unsafeIndex args.scripts scriptInd 
       withKeyWallet key do
         case scriptMode of
           Lock -> do 
              eTxHash <- try $ payToAlwaysSucceeds scriptHash   
              logShow eTxHash 
              case eTxHash of
                  Right txHash -> liftAff $ AVAR.put txHash $ unsafePartial $ unsafeIndex args.lockedTxHashes scriptInd
                  Left _ -> pure unit
           Unlock -> do 
              log "----------here-----------"
              txHash :: TransactionHash <- liftAff $ AVAR.take $ unsafePartial $ unsafeIndex args.lockedTxHashes scriptInd 
              unlockedTxHash <- try $ spendFromAlwaysSucceeds scriptHash script txHash  
              logShow $ unlockedTxHash 
              log "----------here-----------"
              pure unit


       pure unit

     -- use different wallet for next transaction
     liftEffect $ RF.modify_ next walletInd
  pure unit

     
-- main :: Effect Unit
-- main = do
--   envVars <- getEnvVars
--   let 
--     params = config envVars
--     nWallets = 200
--   scripts <- alwaysTrueScripts
--   launchAff_ do
--     wait <- AVAR.new Wait 
--     scriptInd <- AVAR.new 0 
--     scriptMode <- AVAR.new Lock 
--     lockedTxHashes :: Array (AVAR.AVar TransactionHash) <- fromFoldable <$> replicateM (length scripts) AVAR.empty  
--     let spammersArgs = {
--           wait : wait, 
--           nWallets : nWallets, 
--           backendPars : params,
--           scripts : scripts,
--           lockedTxHashes : lockedTxHashes,
--           scriptInd : scriptInd, 
--           scriptMode : scriptMode 
--         }
--     _ <- forkAff $ spammer spammersArgs 
--     _ <- forkAff $ spammer spammersArgs 
--     pure unit
--
--
--
--
--               
--
--
--         let
--             step randNum iWallet iScript
--              |randNum < 0.01
--        if randomNumber < 0.01 
--      i <- liftEffect $ RF.read walletInd
--      let key = unsafePartial $ unsafeIndex keys i 
--          next x | x == args.nWallets - 1 = 0
--          next x  = x + 1 
--      -- simple transaction - pay to wallet
--      then do
--        let
--          pkh = unsafePartial $ unsafeIndex pkhs (next i)
--        _ <- try $ payFromKeyToPkh key pkh 
--        pure unit
--      -- transaction with a script interaction
--      else do
--        scriptMode :: ScriptMode <- liftAff $ AVAR.take args.scriptMode
--        scriptInd :: Int <- liftAff $ AVAR.take args.scriptInd
--        let 
--          nScripts = length args.scripts 
--          update Lock i | i == nScripts - 1 = Unlock /\ 0
--          update Unlock i | i == nScripts - 1 = Lock /\ 0
--          update x i  = x /\ (i + 1)
--          scriptModeNext /\ scriptIndNext = update scriptMode scriptInd
--        void $ liftAff $ AVAR.put scriptModeNext args.scriptMode
--        void $ liftAff $ AVAR.put scriptIndNext args.scriptInd
--
--        let
--          script /\ scriptHash = unsafePartial $ unsafeIndex args.scripts scriptInd 
--        withKeyWallet key do
--          case scriptMode of
--            Lock -> do 
--               eTxHash <- try $ payToAlwaysSucceeds scriptHash   
--               logShow eTxHash 
--               case eTxHash of
--                   Right txHash -> liftAff $ AVAR.put txHash $ unsafePartial $ unsafeIndex args.lockedTxHashes scriptInd
--                   Left _ -> pure unit
--            Unlock -> do 
--               log "----------here-----------"
--               txHash :: TransactionHash <- liftAff $ AVAR.take $ unsafePartial $ unsafeIndex args.lockedTxHashes scriptInd 
--               unlockedTxHash <- try $ spendFromAlwaysSucceeds scriptHash script txHash  
--               logShow $ unlockedTxHash 
--               log "----------here-----------"
--               pure unit
--
--
--        pure unit
--
--      -- use different wallet for next transaction
--      liftEffect $ RF.modify_ next walletInd
--   pure unit
--
