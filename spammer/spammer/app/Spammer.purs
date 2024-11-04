module Spammer where

import Contract.Prelude
import Spammer.Config

import Cardano.Serialization.Lib (privateKey_generateEd25519, privateKey_toBech32, privateKey_toPublic, publicKey_hash)
import Cardano.Types (NetworkId(..), PaymentPubKeyHash(..), PrivateKey(..))
import Cardano.Types.Address (toBech32)
import Cardano.Types.BigNum (fromInt)
import Cardano.Types.PrivateKey (generate, toPublicKey)
import Cardano.Types.PublicKey (hash)
import Contract.Config (PrivatePaymentKeySource(..), PrivateStakeKeySource(..), WalletSpec(..))
import Contract.Monad (launchAff_, runContract)
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKey, mustPayToPubKeyAddress)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (ownPaymentPubKeyHash)
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.Wallet (Wallet(..), mkKeyWallet)
import Ctl.Internal.Wallet.Spec (mkWalletBySpec)


main :: Effect Unit
main = do
  log "Hi"
  priv_key <- generate 
  let pkh :: PaymentPubKeyHash 
      pkh  = PaymentPubKeyHash $ hash $ toPublicKey $ priv_key
  envVars <- getEnvVars
  log envVars.kupoUrl
  log envVars.walletPath
  let cfg = config envVars 
  launchAff_ $ do 
     runContract cfg do 
        let paySelf = mustPayToPubKey pkh (lovelaceValueOf $ fromInt 5_000_000)   
        txHash <- submitTxFromConstraints mempty paySelf
        log $ show txHash
        pure unit 

