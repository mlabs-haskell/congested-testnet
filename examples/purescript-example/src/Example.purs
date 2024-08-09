module Example where
{- 
  In this example, we use congested testnet and purescript for simple transaction.

  1. To get test ada use next command inside `congested-testnet` repo 
      ```
      nix run .#get-tada  

      ```
      it will generate a pair of keys and request ADA from the faucet.
  2. install spago with purescript 0_14_5 , or use inside this repo 
      ```
      nix develop .#purs

      ```
      it enters nix shell with necessary tools

      run ```npm intall``` to install necessary npm modules 

  3. inside purescript-example folder now we should have key.skey file generated in (1)  

  4. inside purescript-example run 
     ```
      spago run -m Example

     ```
 
-}

import Contract.Prelude

import Contract.Monad (ContractParams, launchAff_, liftedM, runContract)
import Config (config)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (ownPaymentPubKeyHash)
import Data.BigInt as BInt


config' :: ContractParams 
config' = config "key.skey" "congested-testnet.staging.mlabs.city" "congested-testnet.staging.mlabs.city" 1337 1442

main :: Effect Unit 
main = do
  launchAff_ do
    runContract config' do 
       pkh <- liftedM "no pkh" ownPaymentPubKeyHash 
       let
          paySelf = mustPayToPubKey pkh (lovelaceValueOf $ BInt.fromInt 3_000_000)
       txHash <- submitTxFromConstraints mempty paySelf
       log $ show txHash
       _ <- awaitTxConfirmed txHash
       log "transaction submitted successful"

