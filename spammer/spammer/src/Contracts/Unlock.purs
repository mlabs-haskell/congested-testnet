module Spammer.Contracts.Unlock where

import Contract.Prelude

-- newtype UnlockParams = UnlockParams {
--   wallet :: KeyWallet , 
--   validator :: Validator 
--   }
--
-- derive instance Newtype UnlockParams _
-- derive instance Generic UnlockParams _

-- unlock :: UnlockParams -> StateT SpammerEnv Contract Unit 
-- unlock (UnlockParams pars) = lift $ withKeyWallet pars.wallet do
--   mUtxos <- getWalletUtxos
--   let 
--       utxos = do
--         utxos <- mUtxos
--         pure <<< fromFoldable <<< take 1 <<< toUnfoldable $ utxos 
--
--   case utxos of  
--       Nothing -> pure unit 
--       Just utxos -> do 
--         allScriptUtxos <- utxosAt (scriptHashAddress (validatorHash pars.validator) Nothing)
--         selectedScriptUtxo <- liftMaybe (error "can't find any script utxo") (findMax allScriptUtxos)
--         let
--             -- selectedScriptUtxos = fromFoldable <<< take 1 <<< toUnfoldable $ allScriptUtxos 
--             lookups = unspentOutputs allScriptUtxos <> validator pars.validator
--             constraints = mustSpendScriptOutput (selectedScriptUtxo.key) unitRedeemer
--
--         log $ show utxos
--         txId <- submitTxFromConstraints lookups constraints
--         pure unit
--
