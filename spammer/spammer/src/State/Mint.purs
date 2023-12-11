module Spammer.State.Mint where

import Contract.Prelude

import Aeson (Aeson, decodeAeson)
import Contract.Prim.ByteArray (ByteArray(..))
import Contract.Value (lovelaceValueOf, singleton)
import Ctl.Internal.Cardano.Types.Value (AssetClass(..), assetToValue)
import Data.Argonaut (Json)
import Data.BigInt (fromInt)
import Spammer.Query.Scripts ( getMintingPolicy, getValidator, getValue')
import Spammer.Query.Utxos (getUtxos')
import Spammer.Query.Wallet (getWallet')
import Spammer.State.Types (SpammerEnv(..), defaultSpammerEnv)
import Spammer.Utils (liftJsonDecodeError)

updateEnvForMint :: SpammerEnv -> Aff SpammerEnv
updateEnvForMint (SpammerEnv env) = do
  policy <- getMintingPolicy
  wallet <- getWallet'
  utxos <- getUtxos'
  value <- getValue' policy
  pure <<< wrap $ (unwrap defaultSpammerEnv) { policy = policy, wallet = wallet, value = value, utxos = utxos}

